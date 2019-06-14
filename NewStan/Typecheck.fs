module Typecheck

open NewStanSyntax
open Constraints
open Util

type Dict = Map<Ide,Type> 
type FunSignature = (TypePrim list) * (TypeLevel list) * Type
type Signatures = Map<string, FunSignature>

// give Model level, say, to all build-ins
let Buildins : Signatures = Map.map (fun k (ts, r) -> ts, List.map (fun t -> Model) ts, (r, Model)) NewStanSyntax.Primitives


let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let emptyGamma = Map.empty


let ty x: TypePrim = 
    if box x :? double then Real
    elif box x :? int then Int
    elif box x :? bool then Bool
    else failwith "unexpected constant"

let lub (ells: TypeLevel list) =
    assert (List.length ells > 0)
    List.fold (fun s ell -> if s <= ell then ell else s) Data ells

let rec glb (ells: TypeLevel list) =
    assert (List.length ells > 0)
    match ells with
    | l::[] -> l
    | l::ls ->
        let l' = glb ls 
        if l = Data || l' = Data then Data
        elif BaseTypeLevel l && BaseTypeLevel l' then
            if l <= l' then l else l' 
        else Glb [l; l']
    | [] -> failwith "unexpected"
                    

let vectorize (orig_fun: TypePrim list * TypePrim) (vectorized_args: TypePrim list): TypePrim =

    let args, ret = orig_fun
    let zipped = List.zip args vectorized_args
    let k = List.fold (fun s (t, t') -> 
                            match t' with
                            | Array(t'', n) -> if t = t'' then n else s
                            | _ -> s 
                        ) (N 0) zipped

    assert true // TODO: write out correct assertion

    if k = N 0 then ret
    else Array(ret, k)

let typecheck_Prog ((defs, s): NewStanProg): NewStanProg =        

    let rec synth_E (signatures: Signatures) (gamma: Dict) (e: Exp): Type*(Constraint list) =
        match e with
        | Var(x) -> 
            if gamma.ContainsKey(x) then gamma.Item(x), []
            else failwith (sprintf "%s not found in type environment" x)

        | Const(n) -> (ty(n), Data), []

        | Arr(Es) -> 
            let PLs, Cs = List.map (synth_E signatures gamma) Es 
                       |> List.unzip 
            let Ps, Ls = List.unzip PLs
            let c = List.fold (List.append) [] Cs

            // FIXME: What do we do with an empty array?
            assert ( List.length Ps > 0 && List.forall (fun p -> p = List.head Ps) Ps )
        
            (Array(List.head Ps, N (List.length Ps)), Lub(Ls)), c

        | ArrElExp(e1, e2) -> 
            let (tau1, ell1), c1 = synth_E signatures gamma e1
            let tau = match tau1 with 
                      | Array(t, n) -> t
                      | Vector(n) -> Real
                      | Matrix(n1, n2) -> Vector(n1)
                      | _ -> failwith "unexpected"

            let (tau2, ell2), c2 = synth_E signatures gamma e2
            assert (Int <. tau2) 

            (tau, Lub ([ell1; ell2])), (List.append c1 c2)

        | Prim(name, Es) -> 
            let PLs, Cs = List.map (synth_E signatures gamma) Es 
                       |> List.unzip 
            let Ps, Ls = List.unzip PLs
            let c = List.fold (List.append) [] Cs

            let fun_signature = 
                if signatures.ContainsKey name then
                    signatures.Item name
                else failwith (sprintf "function %s not defined" name)
            
            let taus, ells, (tau, ell) = fun_signature

            let tau' = vectorize (taus, tau) (Ps)

            (tau', Lub Ls), c

        | ECall(name, Es) -> 
            let PLs, Cs = List.map (synth_E signatures gamma) Es 
                       |> List.unzip 

            let c = List.fold (List.append) [] Cs
            let taus', ells' = List.unzip PLs

            let fun_signature = 
                if signatures.ContainsKey name then
                    signatures.Item name
                else failwith (sprintf "function %s not defined" name)

            let taus, ells, (tau, ell) = fun_signature
            let tau' = vectorize (taus, tau) (taus')
            let c' = List.map2 (fun l l' -> Leq(l', l)) ells ells' 

            (tau', ell), (List.append c c')

        | Plus(e1, e2) -> synth_E signatures gamma (Prim("+", [e1; e2]))
        | Mul(e1, e2) -> synth_E signatures gamma (Prim("*", [e1; e2]))

    let rec check_E (signatures: Signatures)  (gamma: Dict) (e: Exp) ((tau,ell): Type) : Constraint list =    
        let (tau', ell'), c = synth_E signatures gamma e
        assert (tau' <. tau) // FIXME: does the subtyping go in that direction?
        (Leq(ell',ell))::c 


    let rec synth_D (signatures: Signatures) (gamma: Dict) (d: Dist): Type*(Constraint list) = 
        match d with
        | Dist(name, Es) -> 
            let PLs, Cs = List.map (synth_E signatures gamma) Es 
                       |> List.unzip 
            let Ps, Ls = List.unzip PLs
            let c = List.fold (List.append) [] Cs
            
            let fun_signature = 
                if signatures.ContainsKey name then
                    signatures.Item name
                else failwith (sprintf "function %s not defined" name)

            let taus, ells, (tau, ell) = fun_signature

            let tau' = vectorize (taus, tau) (Ps)

            (tau', Lub Ls), c

        | DCall(name, Es) -> 
            synth_E signatures gamma (ECall(name, Es))

    let rec check_D (signatures: Signatures) (gamma: Dict) (d: Dist) ((tau,ell): Type) : Constraint list =
        let (tau', ell'), c = synth_D signatures gamma d
        assert (tau' <. tau) // FIXME: does the subtyping go in that direction?
        (Leq(ell',ell))::c

    let rec synth_L (signatures: Signatures) (gamma: Dict) (l: LValue): Type*(Constraint list) =
        match l with
        | I(x) -> 
            if gamma.ContainsKey(x) then gamma.Item(x), []
            else failwith (sprintf "%s not found in type environment" x)
        | A(l', e') -> 
            let (tau1, ell1), c1 = synth_L signatures gamma l'
            let tau = match tau1 with 
                      | Array(t, n) -> t
                      | Vector(n) -> Real
                      | Matrix(n1, n2) -> Vector(n1)
                      | _ -> failwith "unexpected"

            let (tau2, ell2), c2 = synth_E signatures gamma e'
            assert (Int <. tau2) 

            (tau, Lub ([ell1; ell2])), (List.append c1 c2)


    let rec synth_S (signatures: Signatures) (gamma: Dict) (S: S): TypeLevel*Dict*(Constraint list) = 
        match S with
        | Assign(lhs, e) -> 
            let (tau, ell), c1 = synth_L signatures gamma lhs
            let c2 = check_E signatures gamma e (tau, ell)
            ell, emptyGamma, (List.append c1 c2)

        | Sample(lhs, d) -> 
            let (tau, ell), ce = synth_L signatures gamma (lhs)                
            let cd = check_D signatures gamma d (tau, Model)

            Model, emptyGamma, (Leq(ell, Model))::(List.append ce cd) // assert (ell <= Model)

        | DataDecl(tp, x, s') -> 
            let gamma' = Map.add x (tp, Data) gamma
            let l', g, c = synth_S signatures gamma' s'
            l', ( Map.add x (tp, Data) g ), c

        | Seq(s1, s2) -> 
            let ell1, gamma1, c1 = synth_S signatures gamma s1
            let ell2, gamma2, c2 = synth_S signatures gamma s2
            (glb [ell1; ell2]), (join gamma1 gamma2), (List.append c1 c2)

        | If(e, s1, s2) ->
            let (tau, ell), ce = synth_E signatures gamma e
            assert (tau = Bool)
            let (gamma1, cs1), (gamma2, cs2) = check_S signatures gamma s1 ell, check_S signatures gamma s2 ell
            ell, emptyGamma, (List.append cs1 cs2 |> List.append ce)
            // FIXME: use correct gammas

        | Skip -> GenQuant, emptyGamma, []

        | Block(env, s') -> 
            let (p, l), x = env
            let gamma' = Map.add x (p, l) gamma
            let l', g, c = synth_S signatures gamma' s'
            let c' =
                if (Set.contains x (assigns s')) then c
                else (Leq(Model, l)) :: c
            l', ( Map.add x (p, l) g ), c'

        | For(env, l, u, s') -> 
            let (p, l), x = env
            let gamma' = Map.add x (p, l) gamma
            synth_S signatures gamma' s'

        | VCall(name, Es) -> 
            let (tau, ell), c = synth_E signatures gamma (ECall(name, Es))
            assert (tau = Unit)
            ell, gamma, c


    and check_S (signatures: Signatures) (gamma: Dict) (s: S) (ell: TypeLevel) : Dict*(Constraint list) = 
        let ell', gamma', c = synth_S signatures gamma s            
        (join gamma gamma'), (Leq(ell, ell'))::c // assert ( ell <= ell' )


    let typecheck_Def (signatures: Signatures) (def: FunDef) : Signatures*(Constraint list) =             
        match def with 
        | FunE (name, args, S, E) -> 
            
            let Ts, _ = List.unzip args
            let Ps, Ls = List.unzip Ts

            let argsgamma = (Map.ofList (List.map Util.flip_tuple args))
            let ell, gamma, cs = synth_S signatures argsgamma S //Data

            let ret, ce = synth_E signatures (join argsgamma gamma) E            
            Map.add name (Ps, Ls, ret) signatures, (List.append cs ce)   

        | FunD (name, args, S, D) -> 
            let Ts, _ = List.unzip args
            let Ps, Ls = List.unzip Ts

            let argsgamma = (Map.ofList (List.map Util.flip_tuple args))
            let _, gamma, cs = synth_S signatures argsgamma S // Data

            let ret, cd = synth_D signatures (join argsgamma gamma) D            
            Map.add name (Ps, Ls, ret) signatures, (List.append cs cd)  

        | FunV (name, args, S, _) -> 
            let Ts, _ = List.unzip args
            let Ps, Ls = List.unzip Ts

            let argsgamma = (Map.ofList (List.map Util.flip_tuple args))
            let ell, gamma, c = synth_S signatures argsgamma S // Data
            let _, def_types = Map.toList gamma 
                            |> List.unzip

            Map.add name (Ps, Ls, (Unit, ell)) signatures, c

    let rename_NewStanProg (dict : Map<Ide, TypeLevel>) (defs, s) =
        
        let rename_arg (((t,l), x):Arg) =
             match l with
             | LevelVar(ln) -> 
                if dict.ContainsKey(ln) then (t, dict.Item(ln)), x
                else (t, GenQuant), x //FIXME: is that a correct assumption?
             | _ -> ((t,l), x)

        let rec rename_S s =
            match s with 
            | Block (a, s') -> Block(rename_arg a, rename_S s')
            | DataDecl(t, x, s') ->  DataDecl(t, x, rename_S s')
            | Seq(s1, s2) -> Seq(rename_S s1, rename_S s2)
            | s' -> s'
        
        let rename_def def = 
            match def with
            | FunE (name, args, s, e) -> FunE(name, (List.map rename_arg args), (rename_S s), e)
            | FunD (name, args, s, d) -> FunD(name, (List.map rename_arg args), (rename_S s), d)
            | FunV (name, args, s, ()) -> FunV(name, (List.map rename_arg args), (rename_S s), ())
             
        
        List.map rename_def defs, rename_S s

    let signatures, cdefs = List.fold (fun (signatures, cs) def -> 
                                            let s', c' = typecheck_Def signatures def
                                            s', List.append cs c') (Buildins, []) defs
    
    let _, c = check_S signatures (Map.empty) s Data  

    let inferred_levels = Constraints.naive_solver (List.append cdefs c)

    //printfn "Inferred type levels:  %A\n" (inferred_levels)

    rename_NewStanProg inferred_levels (defs, s)

    








