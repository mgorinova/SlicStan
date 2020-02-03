module Typecheck

open SlicStanSyntax
open Constraints
open Util

type Gamma = Map<Ide,Type> 
type FunSignature = (TypePrim list) * (TypeLevel list) * Type
type Signatures = Map<string, FunSignature>

let mutable toplevel = true // FIXME: bad style, refactor code whenever there's time
let mutable dv : Ide = ""

// allow all buildins to be used everywhere in the program
// FIXME: not sure that's the right way to do it
let Buildins : Signatures = Map.map (fun k (ts, r) -> ts, List.map (fun t -> Model) ts, (r, Model)) SlicStanSyntax.Primitives


let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let emptyGamma = Map.empty


let ty x: TypePrim = 
    
    if x = double (int x) then Int
    elif box x :? double then Real
    else  failwith "unexpected constant" 
    
    // if box x :? double then Real
    // elif box x :? int then Int
    // elif box x :? bool then Bool
    

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
                    

let rec simplify_type (ell : TypeLevel) : TypeLevel = 
    match ell with
    | Data -> Data
    | Model -> Model
    | GenQuant -> GenQuant
    | Lub ells -> lub (List.map simplify_type ells)
    | Glb ells -> glb (List.map simplify_type ells)
    | _ -> failwith "can't simplify unknown type"

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
    | Message(_, message, s') -> Set.add message (assigns s')
    | Elim(_, _, s') -> assigns s'
    | Generate(_, _, s') -> assigns s'



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
    | Message(_, message, s') -> 
        read_at_level gamma s'

    | Elim((T, d), message, s') -> 
        read_at_level (Map.add d T gamma) s'

    | Generate((T, d), _, s') -> 
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
  
let localisable (gamma: Gamma) (s : S) : Constraint list =
    
    let R = read_at_level gamma s
    let W = assigned_of_level gamma s
    
    map_intersect R W 
        |> Map.fold (fun state _ (lr, lw) -> List.append [Leq(lr, lw); Leq(lw, lr)] state) []

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

        if toplevel // || not local_blocks 
            then ell, emptyGamma, (List.append c1 c2)
        else
            // This means we are shredding to eliminate the variable dv.
            // In this case, we add two constraints to match the respective
            // criteria: 
            
            // 1) Variables become local to the level they belong to. That is,
            //    if x is read at level l, then it must be exactly of level l.
            let c' = []
                //match Map.tryFind (LValueBaseName lhs) read_at_level_set with
                //| Some l -> [Leq(l, ell); Leq(ell, l)]
                //| None ->  []
            
            // 2) If the statement mentions dv at all, then all involved vars 
            //    must be at most of level Model. This makes sure we eliminate 
            //    all mentions of dv.
            let c'' = if Set.contains dv (read_exp e) 
                      then [Leq (ell, Model)]
                      else []
                
            ell, emptyGamma, (List.append c1 c2 |> List.append c' |> List.append c'')   
               
    | Sample(lhs, d) ->       
        let (tau, ell), clhs = synth_L signatures gamma lhs
        
        if toplevel then
            // Previously, we would force all ~ to be of level model:
            let cd = check_D signatures gamma d (tau, Model)
            Model, emptyGamma, (Leq(ell, Model))::(List.append clhs cd) 
            
            // FIXME: the below breaks if statements?  

            // Now, we allow for some ~ to mean random number generation:
            // let (tau', ell'), cd = synth_D signatures gamma d 
            // assert(tau <. tau') 
            // ell', emptyGamma, (( Leq(ell', Lub [ell; Model]) ) :: List.append clhs cd)
        
        else //local_blocks then // third shredding
            
            // This means we are shredding to eliminate the variable dv.
            // In this case, we add a constraint to match the respective
            // criteria: if dv is mentioned in the satetemnt, then all
            // involved vars are at most Model:

            if Set.contains dv (read_dist d)  || Set.contains dv (lhs_to_exp lhs |> read_exp) then
                // cannot be GenQuant                
                let cd = check_D signatures gamma d (tau, Model)
                Model, emptyGamma, (Leq(ell, Model))::(List.append clhs cd) // assert (ell <= Model)

            else                 
                // can be GenQuant
                let (tau', ell'), cd = synth_D signatures gamma d 
                assert(tau <. tau')             
                Glb [ell'; ell], emptyGamma, (List.append clhs cd)

    | Seq(s1, s2) -> 
        let ell1, gamma1, c1 = synth_S signatures gamma s1
        let ell2, gamma2, c2 = synth_S signatures gamma s2
        let c = shreddable gamma s1 s2

        if toplevel || dv = "" then 
            (glb [ell1; ell2]), (join gamma1 gamma2), (List.append c1 c2 |> List.append c)
        else
            let c_local = localisable gamma S
            (glb [ell1; ell2]), (join gamma1 gamma2), (List.append c1 c2 |> List.append c |> List.append c_local)

    | If(e, s1, s2) ->
        let (ell1, gamma1, cs1), (ell2, gamma2, cs2) = synth_S signatures gamma s1, synth_S signatures gamma s2
        let ce = check_E signatures gamma e (Bool, Lub [ell1; ell2])
        
        Glb [ell1; ell2], Map.union gamma1 gamma2, (List.append cs1 cs2 |> List.append ce)

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

    | For(env, low, up, s') -> 
        let (p, l), x = env
        let gamma' = Map.add x (p, l) gamma
        let ell, g, c = synth_S signatures gamma' s'
        ell, g, c

    | Message(var, message, s') -> 
        let gamma' = Map.add (snd var) (fst var) gamma
        let ell, g, c = synth_S signatures gamma' s'
        
        // NB, FIXME: should there be some check that s' is at most level ell' ?
        let ell' = gamma'.Item(message) |> snd
        ell', g, c

    | Elim(var, message, s') -> 
        let gamma' = Map.add (snd var) (fst var) gamma        
        let ell, g, c = synth_S signatures gamma' s'
        
        let ell' = gamma'.Item(message) |> snd
        Lub[ell; ell'], g, c

    | Generate(var, _, s') -> 
        let g, c = check_S signatures (Map.map (fun x T -> if x = snd var then fst T, GenQuant else T) gamma) s' GenQuant
        GenQuant, g, c


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

let typecheck_Prog ((defs, s): SlicStanProg): SlicStanProg =     

    let signatures, cdefs = List.fold (fun (signatures, cs) def -> 
                                            let s', c' = typecheck_Def signatures def
                                            s', List.append cs c') (Buildins, []) defs
    
    let _, c = check_S signatures (Map.empty) s Data  

    let inferred_levels = Constraints.naive_solver (List.append cdefs c)

    //printfn "Inferred type levels:  %A\n" (inferred_levels)

    rename_SlicStanProg inferred_levels (defs, s)

    
let rename_elaborated inferred_levels (gamma : Gamma) (s : S) : Gamma * S =
    let s' = rename_SlicStanProg inferred_levels ([], s) |> snd
    let gamma' : Gamma = 
        Map.map (fun x (tau, ell) -> 
                    match ell with
                    | LevelVar(name) -> 
                        let ell = Map.tryFind name inferred_levels
                        tau, match ell with Some(l) -> l | None -> Data
                    | _ -> tau, ell
                ) gamma
    
    gamma', s'


let typecheck_elaborated gamma s =

    let _, c = check_S Buildins gamma s Data  

    // printfn "Constraints: %A" c

    let inferred_levels = Constraints.naive_solver c    

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








