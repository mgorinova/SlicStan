module ConstraintSimplification

open ConstraintSolver
open SlicStanSyntax

let ground (l:TypeLevel): bool =
    match l with 
    | Data -> true
    | Model -> true
    | GenQuant -> true
    | Lz -> true
    | _ -> false

let make_ground_lub (ls : TypeLevel list): TypeLevel =
    if List.length ls = 1 then List.head ls
    elif (List.contains Lz ls) && (List.contains GenQuant ls) then
        failwith "Could not resolve constraints"
    else List.fold (fun s l -> if s <= l then l else s) Data ls


let make_ground_glb (ls : TypeLevel list): TypeLevel =
    if List.length ls = 1 then List.head ls
    elif (List.contains Lz ls) && (List.contains GenQuant ls) then
        List.filter (fun l -> l <= Model) ls
        |> List.fold (fun s l -> if s <= l then s else l) Model
    else 
        let start = 
            if (List.contains Lz ls) then Lz 
            elif (List.contains GenQuant ls) then GenQuant
            else Model
        List.fold (fun s l -> if s <= l then s else l) start ls

let simplify_constraints (cs: ConstraintInfo list): ConstraintInfo list =

    let filter_data_level c = 
        
        let rec filter_data lev =
            match lev with 
            | Lub xs -> 
                List.map filter_data xs
                |> List.filter (fun ell -> ell = Data |> not) 
                |> fun ells -> 
                    if List.length ells > 1 then Lub ells
                    elif List.length ells = 1 then List.head ells
                    else Data
            | Glb xs -> 
                List.map filter_data xs
                |> fun ells -> 
                    if List.length ells > 1 then Glb ells
                    elif List.length ells = 1 then List.head ells
                    else Data
            | _ -> lev
         
        match c with 
        | Leq(ell1, ell2) -> Leq(filter_data ell1, filter_data ell2)

 

    let tautology (c: Constraint, i: string): bool =
        match c with
        | Leq(l1, l2) -> 
            if (not (l1 <= l2)) then failwith (sprintf "type level constraints cannot be satisfied:\n    %s" i)
            elif ground l1 && ground l2 then true
            else match l1, l2 with
                 | LevelVar(x), LevelVar(y) -> x = y             
                 | l, LevelVar(_) -> l = Data
                 | _ -> false
        | _ -> false

    let rec recursive_lub_var_filtering ls = 
        match ls with
        | [] -> []
        | l::ls' -> 
            match l with
            | Data -> recursive_lub_var_filtering ls' 
            | Lub ls'' -> 
                let ls''' = recursive_lub_var_filtering ls'' 
                if List.forall ground ls'''
                then ( make_ground_lub ls''' ) :: (recursive_lub_var_filtering ls' )
                else List.append (ls''') (recursive_lub_var_filtering ls' )
            | Glb ls'' -> 
                let ls''' = recursive_glb_var_filtering ls'' 
                if List.forall ground ls'''
                then ( make_ground_glb ls''' ) :: (recursive_lub_var_filtering ls' )
                else (Glb ls''') :: (recursive_lub_var_filtering ls' )
            | _ -> l :: (recursive_lub_var_filtering ls' )

    and recursive_glb_var_filtering ls = 
        match ls with
        | [] -> []
        | l::ls' -> 
            match l with
            | Glb ls'' -> 
                let ls''' = recursive_glb_var_filtering ls'' 
                if List.forall ground ls'''
                then ( make_ground_glb ls''' ) :: (recursive_glb_var_filtering ls' )
                else List.append (ls''') (recursive_glb_var_filtering ls' )
            | Lub ls'' -> 
                let ls''' = recursive_lub_var_filtering ls'' 
                if List.forall ground ls'''
                then ( make_ground_lub ls''' ) :: (recursive_glb_var_filtering ls' )
                else (Lub ls''') :: (recursive_glb_var_filtering ls' )
            | _ -> l :: (recursive_glb_var_filtering ls' )

    let rec expand_constraint (c: Constraint): Constraint list =
        match c with
        | Leq (l, Lub ls) -> 
            if List.contains GenQuant ls then 
                List.map (fun li -> Leq(li, GenQuant)) (l :: ls)
                |> List.fold ( fun s li -> List.append s (expand_constraint li)) []

            elif List.contains Lz ls then 
                List.map (fun li -> Leq(li, Lz)) (l :: ls)
                |> List.fold ( fun s li -> List.append s (expand_constraint li)) []

            elif List.forall ground ls then 
                if (List.contains Lz ls) && (List.contains GenQuant ls) 
                then failwith "Could not resolve constraints"

                let gr = make_ground_lub ls
                [ Leq (l, gr) ]
            elif List.length ls = 1 then [ Leq (l, List.head ls) ] 
            else [ c ]
        
        | Leq (l, Glb ls) -> 
            let ls' = recursive_glb_var_filtering ls 
                    |> Set.ofList
                    |> Set.toList
            [ Leq (l, Glb ls') ]
    
        | Leq (Lub ls, l) ->
            if (List.contains Lz ls) && (List.contains GenQuant ls) 
            then failwith "Could not resolve constraints"

            if List.contains GenQuant ls then 
                List.map (fun li -> Leq(GenQuant, li)) (l :: ls)
                |> List.fold ( fun s li -> List.append s (expand_constraint li)) []

            elif List.contains Lz ls then 
                List.map (fun li -> Leq(Lz, li)) (l :: ls)
                |> List.fold ( fun s li -> List.append s (expand_constraint li)) []
            else
                let ls' = recursive_lub_var_filtering ls 
                List.fold ( fun s li -> Leq (li, l) 
                                        |> expand_constraint 
                                        |> List.append s) [] ls'

        | Leq (Glb ls, l) -> 
            if List.exists (fun li -> match li with Data -> true | _ -> false) ls
            then expand_constraint (Leq(Data, l))
            else [c] 

        | _ -> [c]

        
    let x : ConstraintInfo list = 
        List.fold (fun s (c, i) -> 
            let expanded = (expand_constraint c) |> List.map filter_data_level
            List.append s (List.zip expanded (List.replicate (List.length expanded ) i) )) [] cs          

    let y = x 
          |> List.filter (fun i -> tautology i |> not)
          
    y |> Set.ofList |> Set.toList

