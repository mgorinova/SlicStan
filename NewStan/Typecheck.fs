module Typecheck

open NewStanSyntax

type Dict = Map<Ide,Type> 
type Signatures = Map<string, TypePrim list * TypePrim>

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

let (!=) (l1:TypeLevel) (l2:TypeLevel) =
    match l1, l2 with
    | LogProb, LogProb -> false
    | Data, Data -> false
    | Local, Local -> false
    | Model, Model -> false
    | GenQuant, GenQuant -> false 
    | _ -> true

let demote (ell:TypeLevel) =
    match ell with
    | Data -> LogProb
    | Model -> Data
    | Local -> Data
    // FIXME: what do we do to demote to Local too?
    | GenQuant -> Model
    | LogProb -> failwith "cannot demote LogProb type level"

    
let promote (ell:TypeLevel) =
    match ell with
    | LogProb -> Data
    | Data -> Model
    | Model -> GenQuant
    | Local -> GenQuant
    | GenQuant -> failwith "cannot promote GenQuant type level"


let arr_size (tau: TypePrim) : int = 
    match tau with 
    | Array(tauel, n) -> n
    | _ -> 0


let rec typecheck_E (signatures: Signatures) (gamma: Dict) (e': Exp) ((tau,ell): Type): bool  =
    if ell <= LogProb then false
    else match e' with
         | Var(x) -> 
            if gamma.ContainsKey(x) then
                gamma.Item(x) = (tau,ell) || typecheck_E signatures gamma e' (tau,demote(ell))
            else failwith (sprintf "%s not found in type environment" x)

         | Const(n) -> ell = Data || typecheck_E signatures gamma e' (tau,demote(ell))

         | Arr(Es) -> 
            match tau with 
            | Array(tauel, _) -> 
                List.fold (fun s e -> s && typecheck_E signatures gamma e (tauel,ell)) true Es
                || typecheck_E signatures gamma e' (tau,demote(ell))
            | _ -> false

         | ArrElExp(e1, e2) -> 
            // FIXME: what do I do with the size?
            (typecheck_E signatures gamma e1 (Array(tau, 0), ell) &&
             typecheck_E signatures gamma e2 (Int, ell) )
            || typecheck_E signatures gamma e' (tau,demote(ell))

         | Plus(e1, e2) -> typecheck_E signatures gamma (Prim("+", [e1;e2])) (tau, ell)
         | Mul(e1, e2) -> typecheck_E signatures gamma (Prim("*", [e1;e2])) (tau, ell)

         | Prim(name, Es) -> 
            let tauArgs, tauRet = 
                if signatures.ContainsKey name then
                    signatures.Item name
                else failwith (sprintf "function %s not defined" name)

            if Es.Length = tauArgs.Length then
                let pairs = List.zip Es tauArgs

                let n = arr_size tau
                (tau = tauRet) &&
                List.fold (fun s (e,t) -> s && (typecheck_E signatures gamma e (t,ell) || 
                                               (n > 0 && typecheck_E signatures gamma e (Array(t,n) ,ell)))
                          ) true pairs
                || typecheck_E signatures gamma e' (tau,demote(ell)) 

            else false

         | ECall(name, Es) -> typecheck_E signatures gamma (Prim(name, Es)) (tau, ell)

let rec typecheck_LValue signatures (gamma: Dict) (lhs: LValue) ((tau, ell):Type): bool =
    match lhs with
    | A (lhs', e) -> (typecheck_E signatures gamma e (Int, ell)) && (typecheck_LValue signatures gamma lhs' (tau, ell))
    | I(x) -> if gamma.ContainsKey x then
                let tau', ell' = gamma.Item x
                tau = tau' && ell = ell'
              else failwith (sprintf "%s not found in type environment" x)


let rec typecheck_D (signatures: Signatures) (gamma: Dict) (d: Dist) ((tau, ell): Type): bool =   
    match d with
    | Dist(name, Es) -> 
        let tauArgs, tauRet = 
            if signatures.ContainsKey name then
                signatures.Item name
            else failwith (sprintf "function %s not defined" name)

        if Es.Length = tauArgs.Length then
            let pairs = List.zip Es tauArgs

            let n = arr_size tau
            (tau = tauRet) &&
            List.fold (fun s (e,t) -> s && (typecheck_E signatures gamma e (t,ell) || 
                                            (n > 0 && typecheck_E signatures gamma e (Array(t,n) ,ell)))
                        ) true pairs
            || typecheck_D signatures gamma d (tau,demote(ell)) 
            // FIXME: do I need the subsumption (last line) here?

        else false

    | DCall(name, Es) -> typecheck_D signatures gamma (Dist(name, Es)) (tau, ell)


let rec typecheck_S (signatures: Signatures) (gamma: Dict) (s: S) (ell: TypeLevel): bool =    
    match s with
    | Block ((t, x), s') -> 
        typecheck_S signatures (gamma.Add(x,t)) s' ell

    | Sample (x, d) -> 
        if ell = LogProb then 
            if gamma.ContainsKey x then 
                let t,l = gamma.Item x
                (l = Model) && typecheck_D signatures gamma d (t, Model)
            else failwith (sprintf "%s not found in type environment" x)
            // && typecheck_E e, when I rewrite this to be E ~ D
        else false
            
    | DataDecl (tau, x, s') -> 
        if gamma.ContainsKey x then
            let t, l = gamma.Item x
            (t = tau && l = ell && typecheck_S signatures gamma s' ell) 
            || (ell != GenQuant && typecheck_S signatures gamma s (promote ell)) 
        else failwith (sprintf "%s not found in type environment" x) 

    | Assign (lhs, e) -> 
        //suppose lhs is x - a string
        let x = LValueBaseName lhs

        if gamma.ContainsKey x then
            let tau, l = gamma.Item x

            (ell = l && typecheck_E signatures gamma e (tau, l)) 
            || (ell != GenQuant && typecheck_S signatures gamma s (promote ell))

        else failwith (sprintf "%s not found in type environment" x)

    | Seq (s1, s2) -> 
        (typecheck_S signatures gamma s1 ell) && (typecheck_S signatures gamma s2 ell)
        
    | Skip -> true

    | VCall (name, Es) -> failwith "not implemented"

let typecheck_FDefs (defs : FunDef list) : Signatures =
    let swap (a,b) = (b,a)
    let rec _tc (dfs: FunDef list) (acc: (string * (TypePrim list * TypePrim)) list) =
        match dfs with
        | [] -> acc    
        | d::ds ->    
            let this_def : (string * (TypePrim list * TypePrim)) =             
                match d with 
                | FunE (name, args, S, T, E) -> 
                    let gamma = List.map swap args

                    if (typecheck_S Buildins (Map.ofList gamma) S) LogProb 
                    // FIXME: add signatures of already seen user-defined functions too
                    // FIXME: need to check if the return arg is the correct type too
                    then
                        let types = List.map snd gamma 
                                 |> List.map fst 
                        name, (types, fst T)
                    else failwith (sprintf "function %s does not type check" name)
                
                | FunD _ -> failwith "not implemented"
                | FunV _ -> failwith "not implemented"

            _tc ds (this_def :: acc)

    Map.ofList (_tc defs [])


let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])


let typecheck_Prog ((defs, s): NewStanProg): bool =
    let signatures = typecheck_FDefs defs
                  |> join Buildins

    typecheck_S signatures Map.empty s LogProb








