module Enumerate

open SlicStanSyntax
open Elaborate
open Shredding
open Typecheck
open Util

let mutable message_number = 0
let next_message () = 
    message_number <- message_number + 1
    sprintf "m%A" message_number

let is_discrete_parameter W = 
    fun (x, (tp, tl)) -> tl = Model && tp <. Int && (Set.contains x W |> not)

let discrete_vars W gamma = 
    Map.toList gamma 
    |> List.filter (is_discrete_parameter W) 
    |> List.map fst 
    |> Set.ofList

let discrete_vars_S dvs s =
    Set.intersect (reads s ) dvs

let can_reorder s1 s2 = 
    (Set.intersect (assigns s1) (assigns s2) |> Set.isEmpty) &&
    (Set.intersect (reads s1) (assigns s2) |> Set.isEmpty) &&
    (Set.intersect (assigns s1) (reads s2) |> Set.isEmpty)
    

let split1 (ds : Set<Ide>) s =
    
    let rec left s = 
        match s with 
        | Seq(Skip, s') -> left s'
        | Seq(s', Skip) -> left s'
        | Seq(s1, s2) -> 
            let s1', s1'' = left s1
            let s2', s2'' = left s2

            if can_reorder s1'' s2' then Seq(s1', s2'), Seq(s1'', s2'')
            else s1', SofList [s1''; s2'; s2'']

        | _ -> 
            let tmp = reads s
            let a = 5
            if Set.intersect ds (reads s) |> Set.isEmpty 
            then s, Skip
            else Skip, s
            
    let rec right s = 
        match s with 
        | Seq(Skip, s') -> right s'
        | Seq(s', Skip) -> right s'
        | Seq(s1, s2) -> 
            let s1', s1'' = right s1
            let s2', s2'' = right s2

            if can_reorder s1'' s2' then Seq(s1', s2'), Seq(s1'', s2'')
            else s1', SofList [s1''; s2'; s2'']

        | _ ->             
            if Set.intersect ds (reads s) |> Set.isEmpty
            then Skip, s
            else s, Skip
            

    let sl, sm' = left s
    let sm, sr  = right sm'
    sl, sm, sr        

let rec filter_Skips s =
    match s with
    | Seq(Skip, s') -> filter_Skips s'
    | Seq(s', Skip) -> filter_Skips s'
    | Seq(s1, s2) -> Seq(filter_Skips s1, filter_Skips s2)
    | Message(arg, name, s') -> Message(arg, name, filter_Skips s') 
    | Elim(arg, message, s') -> Elim(arg, message, filter_Skips s') 
    | Generate(arg, message, s') -> Generate(arg, message, filter_Skips s') 
    | _ -> s
    

let rec all_dependent (s : S) (vars : Set<Ide>) : Set<Ide> =
    let rec single_pass (s : S) : Set<Ide> = 
        match s with 
        | Assign(L, E) -> 
            if Set.intersect vars (read_exp E) |> Set.isEmpty
            then Set.empty
            else lhs_to_exp L |> read_exp
            
        | Seq(s1, s2) -> Set.union (single_pass s1) (single_pass s2)
        | Message(_, _, s') -> single_pass s'
        | Elim(_, _, s') -> single_pass s'
        | If(_, s1, s2) -> Set.union (single_pass s1) (single_pass s2)
        | For(_, _, _, s') ->  single_pass s'
        | _ -> Set.empty

    let new_vars = single_pass s
    let new_result = Set.union vars new_vars

    if vars = new_result then vars
    else all_dependent s (new_result)


let enum (gamma : Gamma, s : S) (d: Ide) : Gamma * S = 
    
    let sd, sm, sq = Shredding.shred_S gamma s
    let dependent_vars = all_dependent sm (Set.add d Set.empty)

    printfn "Dependent vars: %A" (Set.toList dependent_vars)

    let sm1, sm2, sm22 = split1 (dependent_vars) sm

    let W = assigns sm2
    let dvs = discrete_vars W gamma
           |> Set.union dependent_vars
           |> Set.remove d
    let sm3, sm4, sm5 = split1 dvs sm2

    assert(filter_Skips sm5 = Skip)
    
    let arg = Map.find d gamma, d
    let message_name = next_message() 
    // FIXME: need to check that the new message name is not in Gamma.
    // FIXME: need to add the message to the new Gamma.

    let s' = SlicStanSyntax.SofList [ sd; sm1; Message(arg, message_name, sm3); Elim(arg, message_name, sm4); sm22; Generate(arg, message_name, sm4); sq ]
    let gamma' = Map.add d (fst arg |> fst, GenQuant) gamma

    gamma', filter_Skips s'


