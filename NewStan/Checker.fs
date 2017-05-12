module Checker

open NewStanSyntax
open Types

let sub level =
    match level with
    | DataLevel -> failwith "sub level exception"
    | ModelLevel -> DataLevel
    | GenQuantLevel -> ModelLevel

let rec type_check_E (E: Exp) (level: TypeLevel) (gamma: (Ide * TypeLevel) list) : bool =
    match E with
    | Var(x) -> 
        List.contains (x, level) gamma ||
        if not(level = DataLevel) then
            type_check_E E (sub level) gamma
        else false
    | Const(k) -> 
        level = DataLevel ||
        type_check_E E (sub level) gamma
    | Plus(E1, E2) -> 
        type_check_E E1 level gamma &&
        type_check_E E2 level gamma
    | Mul(E1, E2) ->
        type_check_E E1 level gamma &&
        type_check_E E2 level gamma
    | Prim(_, Es) -> 
        List.map (fun e -> type_check_E e level gamma) Es 
        |> List.contains false
        |> not

let type_check_D (D: Dist) (level: TypeLevel) (gamma: (Ide * TypeLevel) list) : bool =
    match D with 
    | Dist(_, Es) -> 
        List.map (fun e -> type_check_E e level gamma) Es 
        |> List.contains false 
        |> not

let rec type_check_S (S: S) (level: TypeLevel) (gamma: (Ide * TypeLevel) list) : bool =
    match S with
    | Skip -> true
    | Seq(S1, S2) -> 
        match S1 with
        (*| Data(x) -> 
            List.contains (x, DataLevel) gamma && 
            type_check_S S2 level gamma
            //type_check_S S2 level ((x, DataLevel)::gamma)*)

        | Assign(x, E) ->

            if List.contains (x, DataLevel) gamma then 
                 type_check_E E DataLevel gamma &&
                 type_check_S S2 level gamma
            elif List.contains (x, ModelLevel) gamma then 
                 type_check_E E ModelLevel gamma &&
                 type_check_S S2 level gamma
            elif List.contains (x, GenQuantLevel) gamma then 
                 type_check_E E GenQuantLevel gamma &&
                 type_check_S S2 level gamma
            else false

            (*if type_check_E E GenQuantLevel gamma then
                type_check_S S2 level ((x, GenQuantLevel)::(gamma))
            elif type_check_E E DataLevel gamma then
                type_check_S S2 level ((x, DataLevel)::(gamma))
            elif type_check_E E ModelLevel gamma then
                type_check_S S2 level ((x, ModelLevel)::(gamma))
            else false*)

        | Sample(x, D) ->
            type_check_D D ModelLevel gamma &&
            List.contains (x, ModelLevel) gamma &&
            type_check_S S2 level gamma
        | _ -> failwith "unexpected"

    | S1 -> type_check_S (Seq(S1, Skip)) level gamma