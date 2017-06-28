﻿module Typecheck

open NewStanSyntax

type Dict = Map<Ide,Type> 
type FunSignature = TypePrim list * TypePrim
type Signatures = Map<string, FunSignature>

let Buildins: Map<string, TypePrim list * TypePrim> = 
    Map.ofList ["normal", ([Real; Real], (Real)); // normal(mu, sigma)
                "gamma", ([Real; Real], (Real)); // gamma(alpha, beta)
                "exp_mod_normal", ([Real; Real; Real], (Real)); // exp_mod_normal(mu, sigma, lambda)
                "student_t", ([Real; Real; Real], (Real)); //student_t(nu, mu, sigma)
                "cauchy", ([Real; Real], (Real)); // cauchy(mu, sigma)
                "+", ([Real; Real], (Real));
                "*", ([Real; Real], (Real));
                ]

let (<=) (l1:TypeLevel) (l2:TypeLevel) =
    match l1, l2 with
    | LogProb, _ -> true
    | Data, LogProb -> false
    | Data, _ -> true
    | Local, LogProb -> false
    | Local, Data -> false
    | Local, _ -> true
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


let arr_size (tau: TypePrim) : int = 
    match tau with 
    | Array(tauel, n) -> n
    | _ -> 0   

let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let emptyGamma = Map.empty

let typecheck_Prog ((defs, s): NewStanProg): Dict =

    let ty x: TypePrim = Real

    let lub (ells: TypeLevel list) =
        assert (List.length ells > 0)
        List.fold (fun s ell -> if s <= ell then ell else s) LogProb ells

    let glb (ells: TypeLevel list) =
        assert (List.length ells > 0)
        List.fold (fun s ell -> if ell <= s then ell else s) GenQuant ells

    let vectorize (orig_fun: FunSignature) (vectorized_args: TypePrim list): TypePrim =
        // TODO: implement this properly
        snd orig_fun
        

    let rec synth_E (signatures: Signatures) (gamma: Dict) (e: Exp): Type =
        match e with
        | Var(x) -> 
            if gamma.ContainsKey(x) then gamma.Item(x)
            else failwith (sprintf "%s not found in type environment" x)

        | Const(n) -> ty(n), Data

        | Arr(Es) -> 
            let Ps, Ls = List.map (synth_E signatures gamma) Es 
                        |> List.unzip

            // FIXME: What do we do with an empty array?
            assert ( List.length Ps > 0 && List.forall (fun p -> p = List.head Ps) Ps )
        
            Array(List.head Ps, List.length Ps), lub(Ls)

        | ArrElExp(e1, e2) -> 
            let tau1, ell1 = synth_E signatures gamma e1
            let tau = match tau1 with Array(t, n) -> t

            let tau2, ell2 = synth_E signatures gamma e2
            assert (tau2 = Int)

            tau, lub ([ell1; ell2])

        | Prim(name, Es) -> 
            let Ps, Ls = List.map (synth_E signatures gamma) Es 
                        |> List.unzip

            let fun_signature = 
                if signatures.ContainsKey name then
                    signatures.Item name
                else failwith (sprintf "function %s not defined" name)

            let tau' = vectorize (fun_signature) (Ps)

            tau', lub Ls

        | ECall(name, Es) -> synth_E signatures gamma (Prim(name, Es))
        | Plus(e1, e2) -> synth_E signatures gamma (Prim("+", [e1; e2]))
        | Mul(e1, e2) -> synth_E signatures gamma (Prim("*", [e1; e2]))

    let rec check_E (signatures: Signatures)  (gamma: Dict) (e: Exp) ((tau,ell): Type)  =    
        let tau', ell' = synth_E signatures gamma e
        assert (tau' = tau && ell' <= ell) 


    let rec synth_D (signatures: Signatures) (gamma: Dict) (d: Dist): Type = 
        match d with
        | Dist(name, Es) -> 
            let Ps, Ls = List.map (synth_E signatures gamma) Es 
                        |> List.unzip

            let fun_signature = 
                if signatures.ContainsKey name then
                    signatures.Item name
                else failwith (sprintf "function %s not defined" name)

            let tau' = vectorize (fun_signature) (Ps)

            tau', lub Ls

        | DCall(name, Es) -> synth_D signatures gamma (Dist(name, Es))

    let rec check_D (signatures: Signatures) (gamma: Dict) (d: Dist) ((tau,ell): Type) =
        let tau', ell' = synth_D signatures gamma d
        assert (tau' = tau && ell' <= ell) 


    let rec typecheck_S (signatures: Signatures) (S:S) : Dict = 

        let rec synth_L (gamma: Dict) (l: LValue): Type =
            match l with
            | I(x) -> 
                if gamma.ContainsKey(x) then gamma.Item(x)
                else failwith (sprintf "%s not found in type environment" x)
            | A(l', e') -> 
                let tau1, ell1 = synth_L gamma l'
                let tau = match tau1 with Array(t, n) -> t

                let tau2, ell2 = synth_E signatures gamma e'
                assert (tau2 = Int)

                tau, lub ([ell1; ell2])


        let rec synth_S (gamma: Dict) (s: S): TypeLevel*Dict = 
            match s with
            | Assign(lhs, e) -> 
                let tau, ell = synth_L gamma lhs
                check_E signatures gamma e (tau, ell)
                ell, emptyGamma

            | Sample(x, d) -> 
                let tau, ell = synth_E signatures gamma (Var(x))
                assert (ell <= Model)
                check_D signatures gamma d (tau, Model)
                LogProb, emptyGamma

            | DataDecl(tp, x, s) -> 
                let gamma' = Map.add x (tp, Data) gamma
                let l, g = synth_S gamma' s
                l, Map.add x (tp, Data) g

            | Seq(s1, s2) -> 
                let ell1, gamma1 = synth_S gamma s1
                let ell2, gamma2 = synth_S gamma s2
                (glb [ell1; ell2]), (join gamma1 gamma2)

            | Skip -> GenQuant, emptyGamma

            | Block(env, s) -> 
                let t, x = env
                let gamma' = Map.add x t gamma
                let l, g = synth_S gamma' s
                l, Map.add x t g

            | VCall(name, Es) -> 
                let Ps, Ls = List.map (synth_E signatures gamma) Es 
                          |> List.unzip

                let taus, tau = 
                    if signatures.ContainsKey name then
                        signatures.Item name
                    else failwith (sprintf "function %s not defined" name)

                assert (tau = Unit)

                glb Ls, emptyGamma

        let rec check_S (gamma: Dict) (s: S) (ell: TypeLevel) = 
            let ell', gamma' = synth_S gamma s
            assert ( ell <= ell' )
            join gamma gamma'

        check_S (Map.empty) S LogProb


    let typecheck_Def (signatures: Signatures) (def: FunDef) : Signatures =             
        match def with 
        | FunE (name, args, S, E) -> 
            
            let Ts, _ = List.unzip args
            let Ps, _ = List.unzip Ts

            let gamma = typecheck_S signatures (BlockOfList (args, S))
            let _, def_types = Map.toList gamma 
                            |> List.unzip
            let def_args, _ = List.unzip def_types
            let def_ret, _ = synth_E signatures gamma E            
            Map.add name (def_args, def_ret) signatures    

        | FunD (name, args, S, D) -> 
            let Ts, _ = List.unzip args
            let Ps, _ = List.unzip Ts

            let gamma = typecheck_S signatures (BlockOfList (args, S))
            let _, def_types = Map.toList gamma 
                            |> List.unzip
            let def_args, _ = List.unzip def_types
            let def_ret, _ = synth_D signatures gamma D            
            Map.add name (def_args, def_ret) signatures  

        | FunV (name, args, S, _) -> 
            let Ts, _ = List.unzip args
            let Ps, _ = List.unzip Ts

            let gamma = typecheck_S signatures (BlockOfList (args, S))
            let _, def_types = Map.toList gamma 
                            |> List.unzip
            let def_args, _ = List.unzip def_types          
            Map.add name (def_args, Unit) signatures  

    let signatures = List.fold (fun signatures def -> typecheck_Def signatures def) Buildins defs
    
    typecheck_S signatures s  

    







