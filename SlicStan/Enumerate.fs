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
    

let rec split1 d s =
    match s with 
    | Seq(Skip, s') -> split1 d s'
    | Seq(s', Skip) -> split1 d s'
    | Seq(s1, s2) -> 
        let s1', s1'' = split1 d s1
        let s2', s2'' = split1 d s2

        if can_reorder s1'' s2' then Seq(s1', s2'), Seq(s1'', s2'')
        else s1, s2

    | _ -> 
        if Set.contains d (reads s) 
        then Skip, s
        else s, Skip


let rec split2 d dvs s = 
    match s with 
    | Seq(Skip, s') -> split2 d dvs s'
    | Seq(s', Skip) -> split2 d dvs s'
    | Seq(s1, s2) -> 
        let s1', s1'' = split2 d dvs s1
        let s2', s2'' = split2 d dvs s2

        if can_reorder s1'' s2' then Seq(s1', s2'), Seq(s1'', s2'')
        else s1, s2

    | _ ->         
        printfn "discrete vars: %A" (discrete_vars_S dvs s)
        if Set.contains d (discrete_vars_S dvs s) && (Set.count (discrete_vars_S dvs s) = 1)
        then s, Skip
        else Skip, s

let rec filter_Skips s =
    match s with
    | Seq(Skip, s') -> filter_Skips s'
    | Seq(s', Skip) -> filter_Skips s'
    | Seq(s1, s2) -> Seq(filter_Skips s1, filter_Skips s2)
    | Message(arg, name, s') -> Message(arg, name, filter_Skips s') 
    | Elim(arg, message, s') -> Elim(arg, message, filter_Skips s') 
    | Generate(arg, message, s') -> Generate(arg, message, filter_Skips s') 
    | _ -> s
    

let enum (gamma : Gamma, s : S) (d: Ide) : Gamma * S = 
    
    let sd, sm, sq = Shredding.shred_S gamma s
    let sm1, sm2 = split1 d sm
    let W = assigns sm2
    let dvs = discrete_vars W gamma
    let sm3, sm4 = split2 d dvs sm2
    
    let arg = Map.find d gamma, d
    let message_name = next_message() 
    // FIXME: need to check that the new message name is not in Gamma.
    // FIXME: need to add the message to the new Gamma.

    let s' = SlicStanSyntax.SofList [ sd; sm1; Message(arg, message_name, sm3); Elim(arg, message_name, sm4); Generate(arg, message_name, sm4); sq ]
    let gamma' = Map.add d (fst arg |> fst, GenQuant) gamma

    gamma', filter_Skips s'


