module Typecheck

open NewStanSyntax

type Dict = Map<Ide,Type> 
type FunSignature = TypePrim list * TypePrim
type Signatures = Map<string, FunSignature>

type Constraint = Leq of TypeLevel * TypeLevel | Eq of TypeLevel * TypeLevel

let Buildins: Map<string, TypePrim list * TypePrim> = 
    Map.ofList ["normal", ([Real; Real], (Real)); // normal(mu, sigma)
                "gamma", ([Real; Real], (Real)); // gamma(alpha, beta)
                "exp_mod_normal", ([Real; Real; Real], (Real)); // exp_mod_normal(mu, sigma, lambda)
                "student_t", ([Real; Real; Real], (Real)); //student_t(nu, mu, sigma)
                "cauchy", ([Real; Real], (Real)); // cauchy(mu, sigma)
                "+", ([Real; Real], (Real));
                "*", ([Real; Real], (Real));
                "exp", ([Real], Real);
                "cholesky_decompose", ([Array(Array(Real, 2), 2)], Array(Array(Real, 2), 2));
                ]

let (<=) (l1:TypeLevel) (l2:TypeLevel) =
    match l1, l2 with
    | LogProb, _ -> true
    | Data, LogProb -> false
    | Data, _ -> true
    | Model, LogProb -> false
    | Model, Data -> false
    | Model, _ -> true
    | _, GenQuant -> true
    | _ -> false

(*let (!=) (l1:TypeLevel) (l2:TypeLevel) =
    match l1, l2 with
    | LogProb, LogProb -> false
    | Data, Data -> false
    | Local, Local -> false
    | Model, Model -> false
    | GenQuant, GenQuant -> false 
    | _ -> true*)


let rec constraints_pretty (cs : Constraint list) =
    
    let rec single_pretty c =
        match c with
        | Leq(l1, l2) -> TLev_pretty l1 + " <= " + TLev_pretty l2 
        | Eq(l1, l2) -> TLev_pretty l1 + " = " + TLev_pretty l2  
    
    match cs with
    | c::cs' -> single_pretty c + "\n" + constraints_pretty cs'
    | [] -> ""

let arr_size (tau: TypePrim) : int = 
    match tau with 
    | Array(tauel, n) -> n
    | _ -> 0   

let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let emptyGamma = Map.empty

let ty x: TypePrim = Real

let lub (ells: TypeLevel list) =
    assert (List.length ells > 0)
    List.fold (fun s ell -> if s <= ell then ell else s) LogProb ells

let glb (ells: TypeLevel list) =
    assert (List.length ells > 0)
    List.fold (fun s ell -> if ell <= s then ell else s) GenQuant ells

let vectorize (orig_fun: FunSignature) (vectorized_args: TypePrim list): TypePrim =

    let args, ret = orig_fun
    let zipped = List.zip args vectorized_args
    let k = List.fold (fun s (t, t') -> 
                            match t' with
                            | Array(t'', n) -> if t = t'' then n else s
                            | _ -> s 
                        ) 0 zipped

    assert true // TODO: write out correct assertion

    if k = 0 then ret
    else Array(ret, k)


let typecheck_Prog ((defs, s): NewStanProg): Dict =        

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
        
            (Array(List.head Ps, List.length Ps), Lub(Ls)), c

        | ArrElExp(e1, e2) -> 
            let (tau1, ell1), c1 = synth_E signatures gamma e1
            let tau = match tau1 with Array(t, n) -> t

            let (tau2, ell2), c2 = synth_E signatures gamma e2
            assert (tau2 = Real) // FIXME: should be assert tau2 = Int

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

            let tau' = vectorize (fun_signature) (Ps)

            (tau', Lub Ls), c

        | ECall(name, Es) -> synth_E signatures gamma (Prim(name, Es))
        | Plus(e1, e2) -> synth_E signatures gamma (Prim("+", [e1; e2]))
        | Mul(e1, e2) -> synth_E signatures gamma (Prim("*", [e1; e2]))

    let rec check_E (signatures: Signatures)  (gamma: Dict) (e: Exp) ((tau,ell): Type) : Constraint list =    
        let (tau', ell'), c = synth_E signatures gamma e
        assert (tau' = tau)
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

            let tau' = vectorize (fun_signature) (Ps)

            (tau', Lub Ls), c

        | DCall(name, Es) -> synth_D signatures gamma (Dist(name, Es))

    let rec check_D (signatures: Signatures) (gamma: Dict) (d: Dist) ((tau,ell): Type) : Constraint list =
        let (tau', ell'), c = synth_D signatures gamma d
        assert (tau' = tau) 
        (Leq(ell',ell))::c

    let rec typecheck_S (signatures: Signatures) (S:S) : Dict*(Constraint list) = 

        let rec synth_L (gamma: Dict) (l: LValue): Type*(Constraint list) =
            match l with
            | I(x) -> 
                if gamma.ContainsKey(x) then gamma.Item(x), []
                else failwith (sprintf "%s not found in type environment" x)
            | A(l', e') -> 
                let (tau1, ell1), c1 = synth_L gamma l'
                let tau = match tau1 with Array(t, n) -> t

                let (tau2, ell2), c2 = synth_E signatures gamma e'
                assert (tau2 = Int)

                (tau, Lub ([ell1; ell2])), (List.append c1 c2)


        let rec synth_S (gamma: Dict) (s: S): TypeLevel*Dict*(Constraint list) = 
            match s with
            | Assign(lhs, e) -> 
                let (tau, ell), c1 = synth_L gamma lhs
                let c2 = check_E signatures gamma e (tau, ell)
                ell, emptyGamma, (List.append c1 c2)

            | Sample(x, d) -> 
                let (tau, ell), ce = synth_E signatures gamma (Var(x))                
                let cd = check_D signatures gamma d (tau, Model)

                LogProb, emptyGamma, (Leq(ell, Model))::(List.append ce cd) // assert (ell <= Model)

            | DataDecl(tp, x, s) -> 
                let gamma' = Map.add x (tp, Data) gamma
                let l, g, c = synth_S gamma' s
                l, (Map.add x (tp, Data) g), c

            | Seq(s1, s2) -> 
                let ell1, gamma1, c1 = synth_S gamma s1
                let ell2, gamma2, c2 = synth_S gamma s2
                (Glb [ell1; ell2]), (join gamma1 gamma2), (List.append c1 c2)

            | Skip -> GenQuant, emptyGamma, []

            | Block(env, s) -> 
                let (p, _), x = env
                let l = LevelVar("l" + x)
                let gamma' = Map.add x (p, l) gamma
                let l, g, c = synth_S gamma' s
                l, Map.add x (p,l) g, c

            | VCall(name, Es) -> 
                let PLs, Cs = List.map (synth_E signatures gamma) Es 
                           |> List.unzip 
                let Ps, Ls = List.unzip PLs
                let c = List.fold (List.append) [] Cs

                let taus, tau = 
                    if signatures.ContainsKey name then
                        signatures.Item name
                    else failwith (sprintf "function %s not defined" name)

                assert (tau = Unit)

                glb Ls, emptyGamma, c

        let rec check_S (gamma: Dict) (s: S) (ell: TypeLevel) : Dict*(Constraint list) = 
            let ell', gamma', c = synth_S gamma s            
            (join gamma gamma'), (Leq(ell, ell'))::c // assert ( ell <= ell' )

        check_S (Map.empty) S LogProb


    let typecheck_Def (signatures: Signatures) (def: FunDef) : Signatures*(Constraint list) =             
        match def with 
        | FunE (name, args, S, E) -> 
            
            let Ts, _ = List.unzip args
            let Ps, _ = List.unzip Ts

            // TODO: deal with introduced constraints

            let gamma, cs = typecheck_S signatures (BlockOfList (args, S))
            let _, def_types = Map.toList gamma 
                            |> List.unzip

            let (def_ret, _), ce = synth_E signatures gamma E            
            Map.add name (Ps, def_ret) signatures, (List.append cs ce)   

        | FunD (name, args, S, D) -> 
            let Ts, _ = List.unzip args
            let Ps, _ = List.unzip Ts

            // TODO: deal with introduced constraints

            let gamma, cs = typecheck_S signatures (BlockOfList (args, S))
            let _, def_types = Map.toList gamma 
                            |> List.unzip

            let (def_ret, _), cd = synth_D signatures gamma D            
            Map.add name (Ps, def_ret) signatures, (List.append cs cd)   

        | FunV (name, args, S, _) -> 
            let Ts, _ = List.unzip args
            let Ps, _ = List.unzip Ts

            // TODO: deal with introduced constraints

            let gamma, c = typecheck_S signatures (BlockOfList (args, S))
            let _, def_types = Map.toList gamma 
                            |> List.unzip

            Map.add name (Ps, Unit) signatures, c

    let signatures, cdefs = List.fold (fun (signatures, cs) def -> 
                                            let s', c' = typecheck_Def signatures def
                                            s', List.append cs c') (Buildins, []) defs
    
    let gamma, c = typecheck_S signatures s  

    printfn "%s" (constraints_pretty (List.append cdefs c))

    gamma

    








