module Constraints

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#r "bin\Debug\NewStanSyntax.exe"
#endif

open NewStanSyntax

type Constraint = Leq of TypeLevel * TypeLevel | Eq of TypeLevel * TypeLevel

let rec constraints_pretty (cs : Constraint list) =
    
    let rec single_pretty c =
        match c with
        | Leq(l1, l2) -> TLev_pretty l1 + " <= " + TLev_pretty l2 
        | Eq(l1, l2) -> TLev_pretty l1 + " = " + TLev_pretty l2  
    
    match cs with
    | c::cs' -> single_pretty c + "\n" + constraints_pretty cs'
    | [] -> ""




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


    let rec expand_constraint (c: Constraint): Constraint list =
        match c with
        | Leq (l, Glb ls) -> List.fold ( fun s li -> Leq (l, li) 
                                                   |> expand_constraint 
                                                   |> List.append s) [] ls
    
        | Leq (Lub ls, l) -> List.fold ( fun s li -> Leq (li, l) 
                                                   |> expand_constraint 
                                                   |> List.append s) [] ls

        | Leq (Glb ls, l) ->         
            if List.exists (fun li -> match li with LogProb -> true | _ -> false) ls
            then [Leq(LogProb, l)]
            elif List.exists (fun li -> match li with Data -> true | _ -> false) ls
            then [Leq(Data, l)]
            else failwith "don't know how to deal with this case!"
        | Leq (_, Lub _) -> failwith "don't know how to deal with this case!"

        | _ -> [c]

    List.fold (fun s c -> List.append s (expand_constraint c)) [] cs
    |> List.filter check_not_tautology_nf


let naive_solver (cs: Constraint list): Map<Ide, TypeLevel> =

    let init_dict_forwards() =
        let folder (s: (Ide*TypeLevel) list) (c:Constraint) =
            match c with
            | Leq(LevelVar(x), LevelVar(y)) -> List.append [(x, Data); (y, Data)] s
            | Leq(LevelVar(x), _) -> (x, Data)::s 
            | Leq(_, LevelVar(x)) -> (x, Data)::s 
            | _ -> s

        List.fold (fun s c -> folder s c) [] cs    
        |> Map.ofList 

    let init_dict_backwards() =
        let folder (s: (Ide*TypeLevel) list) (c:Constraint) =
            match c with
            | Leq(LevelVar(x), LevelVar(y)) -> List.append [(x, GenQuant); (y, GenQuant)] s
            | Leq(LevelVar(x), _) -> (x, GenQuant)::s 
            | Leq(_, LevelVar(x)) -> (x, GenQuant)::s 
            | _ -> s

        List.fold (fun s c -> folder s c) [] cs    
        |> Map.ofList   

    let ground (l:TypeLevel): bool =
        match l with 
        | LogProb -> true
        | Data -> true
        | Model -> true
        | GenQuant -> true
        | _ -> false

    let add_min (x:Ide) (l:TypeLevel) (d:Map<Ide, TypeLevel>): Map<Ide, TypeLevel> =
        if d.ContainsKey(x) then 
            if l <= d.Item(x) then Map.add x l d else d
        else d

    let add_max (x:Ide) (l:TypeLevel) (d:Map<Ide, TypeLevel>): Map<Ide, TypeLevel> =
        if d.ContainsKey(x) then 
            if l <= d.Item(x) then d else Map.add x l d
        else d

    let rec single_pass_forwards (cs: Constraint list) (d: Map<Ide, TypeLevel>)  =
        match cs with
        | [] -> d
        | c::ctail -> 
            match c with 
            | Leq(ell, LevelVar(x)) ->
                let l = if ground ell then ell else match ell with LevelVar(y) -> d.Item(y)
                if not (l = GenQuant) then single_pass_forwards ctail (add_max x l d) 
                else single_pass_forwards ctail d 

            | Leq(LevelVar(x), ell) ->
                let l = if ground ell then ell else match ell with LevelVar(y) -> d.Item(y)
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
            | Leq(LevelVar(x), ell) ->
                let l = if ground ell then ell else match ell with LevelVar(y) -> d.Item(y)
                if not (l = GenQuant) then single_pass_backwards ctail (add_min x l d) 
                else single_pass_backwards ctail d 

            | Leq(ell, LevelVar(x)) ->
                let l = if ground ell then ell else match ell with LevelVar(y) -> d.Item(y)
                let l' = d.Item(x)
                assert (l <= l') 
                single_pass_backwards ctail d 


    let rec multi_pass_backwards (cs: Constraint list) (d: Map<Ide, TypeLevel>) (n:int) =
        if n=0 then d
        else
            let d' = single_pass_backwards cs d 
            multi_pass_backwards cs d' (n-1)


    //printfn "%s" (constraints_pretty cs)
    let filltered = expand_and_filter cs
    printfn "Constraints:\n%s\n" (constraints_pretty filltered)


    let dict_forwards = init_dict_forwards()
    let dict_backwards = init_dict_backwards()
    let n = List.length (Map.toList dict_backwards)

    let forwards = multi_pass_forwards filltered dict_forwards n
    let backwards = multi_pass_backwards filltered dict_backwards n
    
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


    union_dicts forwards backwards

