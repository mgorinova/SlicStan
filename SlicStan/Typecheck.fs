module Typecheck

open SlicStanSyntax
open Constraints
open Util

type Gamma = Map<Ide,Type> 
type FunSignature = (TypePrim list) * (TypeLevel list) * Type
type Signatures = Map<string, FunSignature>

// give Model level, say, to all build-ins
let Buildins : Signatures = Map.map (fun k (ts, r) -> ts, List.map (fun t -> Model) ts, (r, Model)) SlicStanSyntax.Primitives


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


let rec assigns (S: S) : Set<Ide> =
    match S with
    | Decl((_, x), s) -> Set.remove x (assigns s)
    | Sample(x, _) -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns s1) (assigns s2)
    | For(_, _, _, s) -> assigns s
    | Seq(s1, s2) -> Set.union (assigns s1) (assigns s2)
    | Skip -> Set.empty



let rec get_lvalue_level (gamma: Gamma) lhs =
    match lhs with 
    | I(x) -> gamma.Item x |> snd
    | A(lhs', _) -> get_lvalue_level gamma lhs'

let rec read_at_level (gamma: Gamma) (S: S) : Map<Ide, TypeLevel> =

    let rec read_lhs (lhs: LValue) : Set<Ide> =
        match lhs with
        | I(_) -> Set.empty
        | A(lhs', e) -> 
            read_lhs lhs'
            |> Set.union (read_exp e) 

    let map_union_lub a b = Map (seq {
        for KeyValue(k, va) in a do
            match Map.tryFind k b with
            | Some vb -> yield k, Lub [ va; vb ]
            | None    -> yield k, va 
        for KeyValue(k, vb) in b do
            match Map.tryFind k a with
            | Some va -> ()
            | None    -> yield k, vb 
        })

    match S with
    | Assign(lhs, e) -> 
        let l = get_lvalue_level gamma lhs
        let involved_vars = read_lhs lhs |> Set.union (read_exp e) 

        involved_vars 
        |> Set.toList
        |> List.map (fun v -> v, l) 
        |> Map.ofList 

    | Sample(e, d) -> 
        let l = Model
        let involved_vars = read_exp e |> Set.union (read_dist d) 

        involved_vars 
        |> Set.toList
        |> List.map (fun v -> v, l) 
        |> Map.ofList 

    | Seq (s1, s2) -> read_at_level gamma s1 |> map_union_lub (read_at_level gamma s2)
    | If (e, s1, s2) -> 

        let s1_map = read_at_level gamma s1
        let s2_map = read_at_level gamma s2
        
        let all_levels = Map.toList s1_map 
                      |> List.append (Map.toList s2_map)
                      |> List.map snd
                      
        let exp_map = read_exp e 
                   |> Set.toList 
                   |> List.map (fun x -> x, Lub all_levels)
                   |> Map.ofList


        exp_map
        |> map_union_lub s1_map
        |> map_union_lub s2_map
        
    | For (arg, lower, upper, s) -> 
        let s_map = read_at_level gamma s
        // FIXME: need to add constraints for the 
        // loop bounds too.
        s_map

    | Decl (_, s) -> read_at_level gamma s
    | Skip -> Map.empty

let assigned_of_level (gamma: Gamma) (s: S) : Map<Ide, TypeLevel> =
    let involved_vars = assigns s

    involved_vars 
    |> Set.toList
    |> List.map (fun v -> v, (gamma.Item v |> snd))
    |> Map.ofList

let map_intersect a b = Map (seq {
    for KeyValue(k, va) in a do
        match Map.tryFind k b with
        | Some vb -> yield k, (va, vb)
        | None    -> () })

let shreddable (gamma: Gamma) (s1: S) (s2: S): Constraint list = 
    
    // check what's in read_at_level and assignedto_of_level for each pair
    // Then this will record the actual type level of each variable (because it could be 
    // a variable type level)
   
    let R = read_at_level gamma s1
    let W = assigned_of_level gamma s2

    map_intersect R W 
        |> Map.map (fun x (lr, lw) -> Leq(lr, lw))
        |> Map.toList
        |> List.map (fun (_, v) -> v)
            

let typecheck_Prog ((defs, s): SlicStanProg): SlicStanProg =        

    let rec synth_E (signatures: Signatures) (gamma: Gamma) (e: Exp): Type*(Constraint list) =
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

    let rec check_E (signatures: Signatures)  (gamma: Gamma) (e: Exp) ((tau,ell): Type) : Constraint list =    
        let (tau', ell'), c = synth_E signatures gamma e
        assert (tau' <. tau) // FIXME: does the subtyping go in that direction?
        (Leq(ell',ell))::c 


    let rec synth_D (signatures: Signatures) (gamma: Gamma) (d: Dist): Type*(Constraint list) = 
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


    let rec check_D (signatures: Signatures) (gamma: Gamma) (d: Dist) ((tau,ell): Type) : Constraint list =
        let (tau', ell'), c = synth_D signatures gamma d
        assert (tau' <. tau) // FIXME: does the subtyping go in that direction?
        (Leq(ell',ell))::c

    let rec synth_L (signatures: Signatures) (gamma: Gamma) (l: LValue): Type*(Constraint list) =
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


    let rec synth_S (signatures: Signatures) (gamma: Gamma) (S: S): TypeLevel*Gamma*(Constraint list) = 
        match S with
        | Assign(lhs, e) -> 
            let (tau, ell), c1 = synth_L signatures gamma lhs
            let c2 = check_E signatures gamma e (tau, ell)
            ell, emptyGamma, (List.append c1 c2)

        | Sample(e, d) ->                
            let (tau, ell), ce = synth_E signatures gamma e
            let cd = check_D signatures gamma d (tau, Model)
            Model, emptyGamma, (Leq(ell, Model))::(List.append ce cd) // assert (ell <= Model)

        | Seq(s1, s2) -> 
            let ell1, gamma1, c1 = synth_S signatures gamma s1
            let ell2, gamma2, c2 = synth_S signatures gamma s2
            let c = shreddable gamma s1 s2
            (glb [ell1; ell2]), (join gamma1 gamma2), (List.append c1 c2 |> List.append c)

        | If(e, s1, s2) ->
            let (tau, ell), ce = synth_E signatures gamma e
            assert (tau = Bool)
            let (gamma1, cs1), (gamma2, cs2) = check_S signatures gamma s1 ell, check_S signatures gamma s2 ell
            ell, emptyGamma, (List.append cs1 cs2 |> List.append ce)
            // FIXME: use correct gammas

        | Skip -> GenQuant, emptyGamma, []

        | Decl(env, s') -> 
            let (p, l), x = env
            
            assert(Map.containsKey x gamma |> not)

            let gamma' = Map.add x (p, l) gamma
            let l', g, c = synth_S signatures gamma' s'

            if (l = Data) || (assigns s' |> Set.contains x) then
                l', ( Map.add x (p, l) g ), c

            else 
                // This bit is only neccessary for the algoritmic typing rules,
                // in order to make sure that variables that are not explicitly 
                // declared as data, and are unassigned, will be parameters.
                l', ( Map.add x (p, l) g ), (Leq(Model, l))::c

        | For(env, l, u, s') -> 
            let (p, l), x = env
            let gamma' = Map.add x (p, l) gamma
            synth_S signatures gamma' s'


    and check_S (signatures: Signatures) (gamma: Gamma) (s: S) (ell: TypeLevel) : Gamma*(Constraint list) = 
        let ell', gamma', c = synth_S signatures gamma s   
        (join gamma gamma'), (Leq(ell, ell'))::c // assert ( ell <= ell' )


    let typecheck_Def (signatures: Signatures) (def: FunDef) : Signatures*(Constraint list) =             
        match def with 
        | Fun (name, args, S, (T, ret_var)) -> 
            
            let Ts, _ = List.unzip args
            let Ps, Ls = List.unzip Ts

            let argsgamma = (Map.ofList (List.map flip_tuple args)) |> Map.add ret_var T
            let ell, gamma, cs = synth_S signatures argsgamma S 
         
            Map.add name (Ps, Ls, T) signatures, cs   

    let rename_SlicStanProg (dict : Map<Ide, TypeLevel>) (defs, s) =
        
        let rename_arg (((t,l), x):Arg) =
             match l with
             | LevelVar(ln) -> 
                if dict.ContainsKey(ln) then (t, dict.Item(ln)), x
                else (t, GenQuant), x //FIXME: is that a correct assumption?
             | _ -> ((t,l), x)

        let rec rename_S s =
            match s with 
            | Decl (a, s') -> Decl(rename_arg a, rename_S s')
            | Seq(s1, s2) -> Seq(rename_S s1, rename_S s2)
            | s' -> s'
        
        let rename_def def = 
            match def with
            // FIXME: renaming probably not really done properly here
            | Fun (name, args, s, ret_arg) -> Fun(name, (List.map rename_arg args), (rename_S s), rename_arg ret_arg)
             
        
        List.map rename_def defs, rename_S s

    let signatures, cdefs = List.fold (fun (signatures, cs) def -> 
                                            let s', c' = typecheck_Def signatures def
                                            s', List.append cs c') (Buildins, []) defs
    
    let _, c = check_S signatures (Map.empty) s Data  

    let inferred_levels = Constraints.naive_solver (List.append cdefs c)

    //printfn "Inferred type levels:  %A\n" (inferred_levels)

    rename_SlicStanProg inferred_levels (defs, s)

    








