module Constraints

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#r "bin\Debug\SlicStanSyntax.exe"
#endif

open SlicStanSyntax

type Constraint = Leq of TypeLevel * TypeLevel // | Or of Constraint * Constraint

exception CouldNotResolveTypesError 

let ground (l:TypeLevel): bool =
    match l with 
    | Data -> true
    | Model -> true
    | GenQuant -> true
    | _ -> false

let make_ground_lub (ls : TypeLevel list): TypeLevel =
    List.fold (fun s l -> if s <= l then l else s) Data ls

let rec constraints_pretty (cs : Constraint list) =
    
    let rec single_pretty c =
        match c with
        | Leq(l1, l2) -> TLev_pretty l1 + " <= " + TLev_pretty l2 
    
    match cs with
    | c::cs' -> single_pretty c + "\n" + constraints_pretty cs'
    | [] -> ""


let rec recursive_lub_filter ls =
    let ls' = ls
            |> List.map (fun li -> match li with Lub [ell] -> ell | Lub l -> Lub(recursive_lub_filter l) | _ -> li)
            |> List.filter (fun li -> match li with Data -> false | Lub [] -> false | _ -> true) 
            |> Set.ofList |> Set.toList

    if ls' = ls then 
        if List.forall ground ls then [List.fold (fun s l -> if s <= l then l else s) Data ls] else ls 
    else recursive_lub_filter ls'


let expand_and_filter (cs: Constraint list): Constraint list =

    let check_not_tautology_nf (c: Constraint): bool =
        match c with
        | Leq(l1, l2) -> 
            if (not (l1 <= l2)) then failwith "type level constraints cannot be satisfied"
            else match l1, l2 with
                 | LevelVar(x), LevelVar(y) -> not (x = y)             
                 | LevelVar(_), l -> not (l = GenQuant)
                 | l, LevelVar(_) -> not (l <= Data)
                 | _ -> false

    let rec recursive_lub_var_filtering ls = 
        match ls with
        | [] -> []
        | l::ls' -> 
            match l with
            | Data -> recursive_lub_var_filtering ls' 
            | LevelVar(x') -> 
                l :: (recursive_lub_var_filtering ls' )
            | Lub ls'' -> 
                let ls''' = recursive_lub_var_filtering ls'' 
                if List.forall ground ls'''
                then ( make_ground_lub ls''' ) :: (recursive_lub_var_filtering ls' )
                else (Lub ls''') :: (recursive_lub_var_filtering ls' )
            | _ -> l :: (recursive_lub_var_filtering ls' )

    let rec expand_constraint (c: Constraint): Constraint list =
        match c with
        | Leq (LevelVar(x), Lub ls) -> 
            let ls' = recursive_lub_var_filtering ls 
            if List.contains GenQuant ls then []
            elif List.forall ground ls' 
            then 
                let gr = make_ground_lub ls'
                [ Leq (LevelVar(x), gr) ]
            else [ Leq (LevelVar(x), Lub ls') ] 
        
        | Leq (l, Glb ls) -> List.fold ( fun s li -> Leq (l, li) 
                                                   |> expand_constraint 
                                                   |> List.append s) [] ls
    
        | Leq (Lub ls, l) -> 
            if List.contains GenQuant ls then [Leq (GenQuant, l)]
            else
                let ls' = recursive_lub_var_filtering ls 
                let ret = List.fold ( fun s li -> Leq (li, l) 
                                                   |> expand_constraint 
                                                   |> List.append s) [] ls'
                ret

        | Leq (Glb ls, l) ->         
            if List.exists (fun li -> match li with Data -> true | _ -> false) ls
            then expand_constraint (Leq(Data, l))
            elif l = GenQuant then []
            else failwith "don't know how to deal with this case!"

        | _ -> [c]


    // printf "Constrint list: %A" cs

    let x = List.fold (fun s c -> List.append s (expand_constraint c)) [] cs
    let y = x |> List.filter check_not_tautology_nf
    y |> Set.ofList |> Set.toList


let rename_constraints cs (d : Map<Ide, TypeLevel>) =
        let rec inner ell = 
            match ell with 
            | LevelVar(x) -> if Map.containsKey x d then d.Item(x) else ell
            | Lub ls -> Lub (List.map inner ls)
            | Glb ls -> Glb (List.map inner ls)
            | _ -> ell

        List.map (fun c -> match c with Leq(l, l') -> Leq(inner l, inner l')) cs
        |> expand_and_filter


let naive_solver (cs: Constraint list): Map<Ide, TypeLevel> =
    
    let filltered = expand_and_filter cs

    let init_dict_forwards filltered =
        let folder (s: (Ide*TypeLevel) list) (c:Constraint) =
            match c with
            | Leq(LevelVar(x), LevelVar(y)) -> List.append [(x, Data); (y, Data)] s
            | Leq(LevelVar(x), _) -> (x, Data)::s 
            | Leq(_, LevelVar(x)) -> (x, Data)::s 
            | _ -> s

        List.fold (fun s c -> folder s c) [] filltered    
        |> Map.ofList 

    let init_dict_backwards filltered =
        let folder (s: (Ide*TypeLevel) list) (c:Constraint) =
            match c with
            | Leq(LevelVar(x), LevelVar(y)) -> List.append [(x, GenQuant); (y, GenQuant)] s
            | Leq(LevelVar(x), _) -> (x, GenQuant)::s 
            | Leq(_, LevelVar(x)) -> (x, GenQuant)::s 
            | _ -> s

        List.fold (fun s c -> folder s c) [] filltered    
        |> Map.ofList   


    let add_min (x:Ide) (l:TypeLevel) (d:Map<Ide, TypeLevel>): Map<Ide, TypeLevel> =
        if d.ContainsKey(x) then 
            if l <= d.Item(x) then Map.add x l d else d
        else d

    let add_max (x:Ide) (l:TypeLevel) (d:Map<Ide, TypeLevel>): Map<Ide, TypeLevel> =
        if d.ContainsKey(x) then 
            if l <= d.Item(x) then d else Map.add x l d
        else d
            
    let rec gimme_a_type (d : Map<Ide, TypeLevel>) (ell : TypeLevel) (x : Ide) =
        if ground ell then ell 
        else match ell with 
        | LevelVar(y) -> d.Item(y)
        | Lub ls -> 
            if (List.length ls) = 1 then gimme_a_type d (List.head ls) x
            elif (List.length ls) = 0 then Data
            else raise CouldNotResolveTypesError
            
    let rec single_pass_forwards (cs: Constraint list) (d: Map<Ide, TypeLevel>)  =
        match cs with
        | [] -> d
        | c::ctail -> 
            match c with 
            (*| Leq(Lub ls, LevelVar(x)) ->
                let l = try gimme_a_type d (Lub ls) x with 
                | CouldNotResolveTypesError ->    
                    //let cs' = rename_constraints cs d
                    //if List.contains Model ls && List.contains (Leq(LevelVar(x), Model)) cs' 
                    //then Model else GenQuant
                    Model

                if not (l = GenQuant) then single_pass_forwards ctail (add_max x l d) 
                else single_pass_forwards ctail d   

            | Leq(LevelVar(x), Lub ls) -> 
                let l = try gimme_a_type d (Lub ls) x with 
                | CouldNotResolveTypesError ->    
                    //let cs' = rename_constraints cs d
                    //if List.contains Model ls && List.contains (Leq(Model, LevelVar(x))) cs' 
                    //then Model else GenQuant
                    Model

                let l' = d.Item(x)                
                assert (l' <= l)
                single_pass_forwards ctail d *)

            | Leq(ell, LevelVar(x)) ->
                let l = gimme_a_type d ell x
                if not (l = GenQuant) then single_pass_forwards ctail (add_max x l d) 
                else single_pass_forwards ctail d 

            | Leq(LevelVar(x), ell) ->
                let l = gimme_a_type d ell x
                let l' = d.Item(x)                
                assert (l' <= l)
                single_pass_forwards ctail d 
                

    let rec multi_pass_forwards (cs: Constraint list) (d: Map<Ide, TypeLevel>) (n:int) =
        if n=0 then d
        else
            let d' = single_pass_forwards cs d 
            multi_pass_forwards cs d' (n-1)


    let rec single_pass_backwards (cs: Constraint list) (d: Map<Ide, TypeLevel>)  =
        match cs with
        | [] -> d
        | c::ctail -> 
            match c with 
            (*| Leq(Lub ls, LevelVar(x)) ->
                let l = try gimme_a_type d (Lub ls) x with 
                | CouldNotResolveTypesError ->    
                    //let cs' = rename_constraints cs d
                    //if List.contains Model ls && List.contains (Leq(LevelVar(x), Model)) cs' 
                    //then Model else GenQuant
                    Model

                if not (l = GenQuant) then single_pass_forwards ctail (add_min x l d) 
                else single_pass_forwards ctail d   

            | Leq(LevelVar(x), Lub ls) -> 
                let l = try gimme_a_type d (Lub ls) x with 
                | CouldNotResolveTypesError ->    
                    let cs' = rename_constraints cs d
                    //if List.contains Model ls && List.contains (Leq(Model, LevelVar(x))) cs' 
                    //then Model 
                    //elif List.contains (Leq(GenQuant, LevelVar(x))) cs' 
                    //then GenQuant
                    //else Data
                    Model 

                let l' = d.Item(x)                
                assert (l' <= l)
                single_pass_forwards ctail d *)

            | Leq(LevelVar(x), ell) ->
                let l = gimme_a_type d ell x 
                if not (l = GenQuant) then single_pass_backwards ctail (add_min x l d) 
                else single_pass_backwards ctail d 

            | Leq(ell, LevelVar(x)) ->
                let l = gimme_a_type d ell x
                let l' = d.Item(x)                
                if l <= l' then () else raise CouldNotResolveTypesError 
                single_pass_backwards ctail d 


    let rec multi_pass_backwards (cs: Constraint list) (d: Map<Ide, TypeLevel>) (n:int) =
        if n=0 then d
        else
            let d' = single_pass_backwards cs d 
            multi_pass_backwards cs d' (n-1)
            
            
    let resolve_quick (cs : Constraint list) = 
        let rec model_equality_filter (cs : Constraint list) = 
            let lub_cs_left, rest1 = List.partition (
                fun c -> match c with 
                        | Leq (Lub ls, LevelVar x) -> 
                            List.contains Model ls && List.contains (Leq (LevelVar x, Model)) cs
                        | Leq (Model, LevelVar _) -> true
                        | _ -> false ) cs
            let vars_left = List.map (fun c -> match c with Leq (_ , LevelVar x) -> x) lub_cs_left

            let lub_cs_right, rest = List.partition (
                fun c -> match c with 
                        | Leq (LevelVar x, Lub ls) -> 
                            List.contains Model ls && List.contains (Leq (LevelVar x, Model)) cs
                        | Leq (LevelVar _, Model) -> true
                        | _ -> false ) rest1
            let vars_right = List.map (fun c -> match c with Leq (LevelVar x, _) -> x) lub_cs_right
            
            let model_vars = 
                List.filter (fun v -> List.contains (Leq(LevelVar v, Model)) cs) vars_left 
                |> List.append ( List.filter (fun v -> List.contains (Leq(Model, LevelVar v)) cs) vars_right )
                
            let model_dict = List.map (fun v -> v, Model) model_vars

            let dict = Map.ofList model_dict 
            let new_cs = rename_constraints cs dict
        
            if List.isEmpty model_vars then cs, []
            else 
                let cs', vars' = 
                    new_cs 
                    |> expand_and_filter
                    |> model_equality_filter

                cs', List.append vars' model_vars 
            
        
        let cs', rest' = List.partition (fun c -> match c with Leq(LevelVar _, Data) -> true | _ -> false) cs
        let data_eq_vars = List.map (fun c -> match c with Leq(LevelVar x, Data) -> x) cs'
        let data_eq_vars_map = Map.ofList (List.map (fun v -> v, Data) data_eq_vars)
        let data_cs = rename_constraints rest' data_eq_vars_map
    
        let cs'', rest'' = List.partition (fun c -> match c with Leq(GenQuant, LevelVar _) -> true | _ -> false) data_cs
        let gq_eq_vars = List.map (fun c -> match c with Leq(GenQuant, LevelVar x) -> x) cs''
        let gq_eq_vars_map = Map.ofList (List.map (fun v -> v, GenQuant) gq_eq_vars)
        let gq_cs = rename_constraints rest'' gq_eq_vars_map

        let model_eq_cs, model_eq_vars = model_equality_filter gq_cs
        let model_eq_vars_map = Map.ofList (List.map (fun v -> v, Model) model_eq_vars)

        
        let all = Map(Seq.concat [ (Map.toSeq model_eq_vars_map) ; 
                                   (Map.toSeq gq_eq_vars_map) ; 
                                   (Map.toSeq data_eq_vars_map) ])

        let res_cs = rename_constraints model_eq_cs all
        let res_cs2 =  List.map (
                        fun c -> match c with
                                 | Leq( LevelVar x, Lub [LevelVar y; Model] ) ->
                                    if List.contains (Leq (Model, LevelVar y)) res_cs
                                    then Leq( LevelVar x, LevelVar y )
                                    else c
                                 | Leq( LevelVar x, Lub [Model; LevelVar y] ) ->
                                    if List.contains (Leq (Model, LevelVar y)) res_cs
                                    then Leq( LevelVar x, LevelVar y )
                                    else c
                                 | _ -> c ) res_cs
        res_cs2, all
        

    let quick_cs, quick_vars = resolve_quick filltered

    let dict_forwards = init_dict_forwards quick_cs
    let dict_backwards = init_dict_backwards quick_cs
    let n = List.length (Map.toList dict_backwards)

    let forwards = multi_pass_forwards quick_cs dict_forwards n
    let backwards = multi_pass_backwards quick_cs dict_backwards n
    
    //printfn "forwards: %A" forwards
    //printfn "backwards: %A" backwards

    let union_dicts (f:Map<Ide, TypeLevel>) (b:Map<Ide, TypeLevel>) =
        let func (k: Ide) (lf: TypeLevel) =
            let lb = b.Item(k)
            match lf, lb with
            | Model, l -> l
            | l, Model -> l
            | Data, _ -> Data
            | _ -> failwith "unexpected result when solving type level constraints" 

        Map.map func f

    
    let rest_map = union_dicts forwards backwards

    Map(Seq.concat [ (Map.toSeq quick_vars) ; (Map.toSeq rest_map) ])
    
