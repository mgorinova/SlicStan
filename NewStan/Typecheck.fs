module Typecheck

open NewStanSyntax

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
