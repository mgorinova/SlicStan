module Typecheck

open SlicStanSyntax
open ConstraintSimplification
open ConstraintSolver
open Util
open System.Xml.Linq


type Gamma = Map<Ide,Type> 
type FunSignature = (TypePrim list) * (TypeLevel list) * Type
type Signatures = Map<string, FunSignature>


let mutable toplevel = true // FIXME: bad style, refactor code whenever there's time
let mutable dv : Ide = ""

// allow all buildins to be used everywhere in the program
//let Buildins : Signatures = Map.map (fun k (ts, r) -> ts, List.map (fun t -> Model) ts, (r, Model)) SlicStanSyntax.Primitives
let Buildins : Signatures = Map.map (fun k (ts, r) -> ts, List.map (fun t -> GenQuant) ts, (r, Data)) SlicStanSyntax.Primitives

let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let emptyGamma = Map.empty


let ty x: TypePrim =     
    if x = double (int x) then Int
    elif box x :? double then Real
    else  failwith "unexpected constant" 


let lub (ells: TypeLevel list) =
    assert (List.length ells > 0)
    let ret = List.fold (fun s ell -> if s <= ell then ell else s) Data ells
    ret


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
                    

let rec simplify_type (ell : TypeLevel) : TypeLevel = 
    match ell with
    | Data -> Data
    | Model -> Model
    | GenQuant -> GenQuant
    | Lz -> Lz
    | Lub ells -> lub (List.map simplify_type ells)
    | Glb ells -> glb (List.map simplify_type ells)
    | _ -> failwith "can't simplify unknown type"


/// Vectorizes a function to be used in type checking
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


/// Computes W(S): the set of variables that are assigned to in S.
let rec assigns (S: S) : Set<Ide> =
    match S with
    | Decl((_, x), s) -> Set.remove x (assigns s)
    | Sample(x, _) -> Set.empty
    | Factor _ -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns s1) (assigns s2)
    | For(_, _, _, s) -> assigns s
    | Seq(s1, s2) -> Set.union (assigns s1) (assigns s2)
    | Skip -> Set.empty
    | Phi((t, name), _, s') -> 
        Set.add name (assigns s')
        // Set.add name (Set.empty)
    | Elim(_, s') -> 
        // We assume any variables assinged inside Elim are local    
        // assigns s'
        Set.empty
    | Gen(_, s') -> 
        // We assume any variables assinged inside Gen are local    
        // assigns s'
        Set.empty

let rec get_lvalue_level (gamma: Gamma) lhs =
    match lhs with 
    | I(x) -> gamma.Item x |> snd
    | A(lhs', _) -> get_lvalue_level gamma lhs'


/// Returns the highest level each variable is read at in S
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

    | Sample(lhs, d) ->         
        let involved_vars = read_lhs lhs |> Set.union (read_dist d) |> Set.add (LValueBaseName lhs) |> Set.toList
        let l = Lub (List.map (fun v -> Map.find v gamma |> snd) involved_vars)

        involved_vars
        |> List.map (fun v -> v, l) 
        |> Map.ofList 

    | Factor(e) ->
        let involved_vars = read_exp e |> Set.toList
        let l = Lub (List.map (fun v -> Map.find v gamma |> snd) involved_vars)

        involved_vars
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
        let s_map = read_at_level (Map.add (snd arg) (fst arg) gamma) s
                 |> Map.remove (snd arg)
        // FIXME: need to add constraints for the 
        // loop bounds too.
        s_map 

    | Decl (_, s) -> read_at_level gamma s
    | Skip -> Map.empty
    | Phi(_, args, s') -> 
        read_at_level gamma s'

    | Elim((T, d), s') -> 
        read_at_level (Map.add d T gamma) s'

    | Gen((T, d), s') -> 
        let l = GenQuant
        let involved_vars = reads s' 
        involved_vars 
        |> Set.toList
        |> List.map (fun v -> v, l) 
        |> List.filter (fun (x, _) -> x <> d) 
        |> Map.ofList 


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


let shreddable (gamma: Gamma) (s1: S) (s2: S): ConstraintInfo list =     
    // check what's in read_at_level and assignedto_of_level for each pair
    // Then this will record the actual type level of each variable (because it could be 
    // a variable type level)
   
    let R = read_at_level gamma s1
    let W = assigned_of_level gamma s2

    map_intersect R W 
        |> Map.map (fun x (lr, lw) -> Leq(lr, lw))
        |> Map.toList
        |> List.map (fun (_, v) -> v, sprintf "Shreddable(%A, %A)" (S_pretty "" s1) (S_pretty "" s2))


let include_target_exp_constraint (gamma: Gamma) (s: S) : TypeLevel = 
    let ells = read_at_level gamma s |> Map.toList |> List.map snd
    let ell = Lub ells
    ell


let rec synth_E (signatures: Signatures) (gamma: Gamma) (e: Exp): Type*(ConstraintInfo list) =
    match e with
    | Var(x) -> 
        if gamma.ContainsKey(x) then gamma.Item(x), []
        else failwith (sprintf "%s not found in type environment" x)

    | Const(n) -> (ty(n), Data), [] // corresponds to (ty(n), L1)

    | Arr(Es) -> 
        let PLs, Cs = List.map (synth_E signatures gamma) Es 
                    |> List.unzip 
        let Ps, Ls = List.unzip PLs
        let c = List.fold (List.append) [] Cs

        // FIXME: What do we do with an empty array?
        assert ( List.length Ps > 0 && List.forall (fun p -> p = List.head Ps) Ps ) // assert all primitive types are the same
        
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
        let c' = List.map2 (fun l l' -> Leq(l', l), sprintf "(ECall): %A" (E_pretty e)) ells ells' 

        (tau', ell), (List.append c c')

    | Plus(e1, e2) -> synth_E signatures gamma (Prim("+", [e1; e2]))
    | Mul(e1, e2) -> synth_E signatures gamma (Prim("*", [e1; e2]))


let rec check_E (signatures: Signatures)  (gamma: Gamma) (e: Exp) ((tau,ell): Type) : ConstraintInfo list =    
    let (tau', ell'), c = synth_E signatures gamma e
    assert (tau' <. tau) // FIXME: does the subtyping go in that direction?
    (Leq(ell',ell), sprintf "(ESub): %A" (E_pretty e))::c 


let rec synth_D (signatures: Signatures) (gamma: Gamma) (d: Dist): Type*(ConstraintInfo list) = 
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


let rec check_D (signatures: Signatures) (gamma: Gamma) (d: Dist) ((tau,ell): Type) : ConstraintInfo list =
    let (tau', ell'), c = synth_D signatures gamma d
    //assert (tau' <. tau) // FIXME: does the subtyping go in that direction?
    (Leq(ell',ell), sprintf "(DSub): %A" (D_pretty d))::c


let rec synth_L (signatures: Signatures) (gamma: Gamma) (l: LValue): Type*(ConstraintInfo list) =
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


let rec synth_S (signatures: Signatures) (gamma: Gamma) (S: S): TypeLevel*Gamma*(ConstraintInfo list) = 
    match S with
    | Assign(lhs, e) -> 
        let (tau, ell), c1 = synth_L signatures gamma lhs
        let c2 = check_E signatures gamma e (tau, ell)
        ell, emptyGamma, (List.append c1 c2)   
    
    | Factor(e) -> 
        if toplevel then 
            Model, emptyGamma, check_E signatures gamma e (Real, Model) 
        else            
            let ell = LevelVar(next())
            let c = check_E signatures gamma e (Real, ell) 
            ell, emptyGamma, c

    | Sample(lhs, d) ->       
        let (tau, ell'), clhs = synth_L signatures gamma lhs

        if toplevel then
            // Previously, we would force all ~ to be of level model:
            // let cd = check_D signatures gamma d (tau, Model)
            // Model, emptyGamma, (Leq(ell, Model), sprintf "(Sample): %A" (S_pretty "" S))::(List.append clhs cd) 
            
            // Now, we allow for some ~ to mean random number generation:
            let ell = Lub [ell'; Model]
            let cd = check_D signatures gamma d (tau, ell)
            ell, emptyGamma, (List.append clhs cd)

        else
            let ell = LevelVar(next())
            let ce = check_E signatures gamma (lhs_to_exp lhs) (Real, ell) 
            let cd = check_D signatures gamma d (Real, ell) 
            ell, emptyGamma, List.append ce cd

    | Seq(s1, s2) -> 
        let ell1, gamma1, c1 = synth_S signatures gamma s1
        let ell2, gamma2, c2 = synth_S signatures gamma s2
        let c = shreddable gamma s1 s2
        
        (glb [ell1; ell2]), (join gamma1 gamma2), (List.append c1 c2 |> List.append c)

    | If(e, s1, s2) ->
        let (ell1, gamma1, cs1), (ell2, gamma2, cs2) = synth_S signatures gamma s1, synth_S signatures gamma s2
        let ce = check_E signatures gamma e (Bool, Lub [ell1; ell2])        
        Glb [ell1; ell2], Map.union gamma1 gamma2, (List.append cs1 cs2 |> List.append ce)

    | Skip -> 
        if toplevel 
        then GenQuant, emptyGamma, []
        else
            let ell = LevelVar(next())
            ell, emptyGamma, []

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
            l', ( Map.add x (p, l) g ), (Leq(Model, l), sprintf "(Decl): %s" (S_pretty "" S))::c

    | For(env, low, up, s') -> 
        let (p, l), x = env
        let gamma' = Map.add x (p, l) gamma
        let ell, g, c = synth_S signatures gamma' s'
        ell, g, c

    | Phi(var, args, s') -> 

        // let (tau, ell), c1 = synth_L signatures gamma lhs
        // let c2 = check_E signatures gamma e (tau, ell)
        // ell, emptyGamma, (List.append c1 c2) 

        let gamma' = List.fold (fun g x -> Map.add x (Int, Data) g ) gamma args
        let _, g, c = synth_S signatures gamma' s'

        let R = read_at_level gamma' s'

        let lm = gamma.[snd var] |> snd

        let cr = 
            Map.map (fun k lr -> Leq(lr, lm)) R
            |> Map.toList    
            |> List.map (fun (k, v) -> v, sprintf "Phi(%A) reads %A" (snd var) k )

        lm, g, List.append c cr

    | Elim(var, s') -> 
        let gamma' = Map.add (snd var) (fst var) gamma        
        let _, g, c = synth_S signatures gamma' s'    
        let ell = include_target_exp_constraint gamma' s'      
        
        ell, g, c

    | Gen(var, s') -> 
        //let g, c = check_S signatures (Map.map (fun x T -> if x = snd var then fst T, GenQuant else T) gamma) s' GenQuant
        // FIXME
        GenQuant, gamma, []


and check_S (signatures: Signatures) (gamma: Gamma) (s: S) (ell: TypeLevel) : Gamma*(ConstraintInfo list) = 
    let ell', gamma', c = synth_S signatures gamma s   
    (join gamma gamma'), (Leq(ell, ell'), sprintf "(SSub): %s" (S_pretty "" s))::c // assert ( ell <= ell' )


let typecheck_Def (signatures: Signatures) (def: FunDef) : Signatures*(ConstraintInfo list) =             
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
        // FIXME: renaming probably not really done properly here ?
        | Fun (name, args, s, ret_arg) -> Fun(name, (List.map rename_arg args), (rename_S s), rename_arg ret_arg)         
        
    List.map rename_def defs, rename_S s


let get_level_vars (gamma: Gamma) = 
    Map.toList gamma
    |> List.map snd // get list of types
    |> List.map snd // get list of level types
    |> List.map (fun ell -> match ell with LevelVar x -> x | _ -> "")
    |> List.filter (fun x -> x = "" |> not)
    |> Set.ofList
    |> Set.toList


let typecheck_Prog ((defs, s): SlicStanProg): SlicStanProg =     

    let signatures, cdefs = List.fold (fun (signatures, cs) def -> 
                                            let s', c' = typecheck_Def signatures def
                                            s', List.append cs c') (Buildins, []) defs
    
    let gamma, c = check_S signatures (Map.empty) s Data  

    let vars = get_level_vars gamma
    let constr = List.append cdefs c |> simplify_constraints
    
    let inferred_levels = ConstraintSolver.resolve(constr, vars) 

    rename_SlicStanProg inferred_levels (defs, s)

    
let rename_elaborated inferred_levels (gamma : Gamma) (s : S) : Gamma * S =
    let s' = rename_SlicStanProg inferred_levels ([], s) |> snd
    let gamma' : Gamma = 
        Map.map (fun x (tau, ell) -> 
                    match ell with
                    | LevelVar(name) -> 
                        let ell = Map.tryFind name inferred_levels
                        tau, match ell with 
                             | Some(l) -> l 
                             | None -> if toplevel then Data else GenQuant
                    | _ -> tau, ell
                ) gamma
    
    gamma', s'


let extract_vars_from_constraints constr = 
    
    let rec single ell =
        match ell with 
        | LevelVar x -> [x]
        | Lub xs -> List.map single xs |> List.fold List.append [] 
        | Glb xs -> List.map single xs |> List.fold List.append [] 
        | _ -> []

    constr 
    |> List.map (fun c -> match c with Leq(ell1, ell2) -> List.append (single ell1) (single ell2))  
    |> List.fold List.append [] 


let typecheck_elaborated gamma s =

    let _, c_typing = check_S Buildins gamma s Data  
        
    //let c = List.append c_typing c_local
    let c = c_typing

    // This bit is only neccessary for the algoritmic typing rules,
    // in order to make sure that variables that are not explicitly 
    // declared as data, and are unassigned, will be parameters.
    //let W = assigns s
    //let extra = Map.toList gamma
    //          |> List.filter (fun (x, (_, tl)) -> (Set.contains x W |> not ) && (tl = Data |> not))
    //          |> List.map (fun (_, (_, tl)) -> Leq(Model, tl), "extra")
     
    let vars = get_level_vars gamma

    let inferred_levels = 
        if toplevel then
            let constr = c // |> simplify_constraints
            let all_vars = extract_vars_from_constraints (List.map fst constr)
                        |> List.append vars
                        |> Set.ofList |> Set.toList
                
            ConstraintSolver.resolve(constr, all_vars)
            
        else
            let constr = c // |> simplify_constraints
            
            let all_vars = extract_vars_from_constraints (List.map fst constr)
                        |> List.append vars
                        |> Set.ofList |> Set.toList

            ConstraintSolver.resolve_semilattice(constr, all_vars)
            

    rename_elaborated inferred_levels gamma s 


let rec recursive_lub_filter ls =
    let ls' = ls
            |> List.map (fun li -> match li with Lub [ell] -> ell | Lub l -> Lub(recursive_lub_filter l) | _ -> li)
            |> List.filter (fun li -> match li with Data -> false | Lub [] -> false | _ -> true) 
            |> Set.ofList |> Set.toList

    if ls' = ls then ls else recursive_lub_filter ls'

(*let rec recursive_glb_filter ls =
        let ls' = ls
               |> List.map (fun li -> match li with Lub [ell] -> ell | Lub l -> Lub(recursive_lub_filter l) | _ -> li)
               |> List.filter (fun li -> match li with Data -> false | Lub [] -> false | _ -> true) 
               |> Set.ofList |> Set.toList 

        if ls' = ls then ls else recursive_lub_filter ls' *)


let rec simplify_level (level : TypeLevel) : TypeLevel =
    match level with 
    | Lub ls -> 
        match recursive_lub_filter ls with
        | [] -> Data
        | [l] -> l
        | l::lss -> 
            let next = simplify_level (Lub lss)
            if (next <= l) then l else next // FIXME: does this break if the level is a levelvar or lub/glb?
    | Glb ls -> failwith "not implemented"
    | _ -> level








