module Shredding

open SlicStanSyntax
open Typecheck
open Elaborate

let rec skippify S = 
    match S with
    | Seq(Skip, Skip) -> Skip
    | If(_, Skip, Skip) -> Skip
    | For(_, _, _, Skip) -> Skip
    | _ -> S

let rec shred_S (gamma: Gamma) (S: S) : (S * S * S) =
    match S with 
    | Assign(lhs, e) -> 
        let base_name = LValueBaseName lhs
        match (Map.gammaItemTypeLevel base_name gamma) with
        | Data -> Assign(lhs, e), Skip, Skip
        | Model -> Skip, Assign(lhs, e), Skip 
        | GenQuant -> Skip, Skip, Assign(lhs, e)
        | _ -> failwith "something went terribly worng"

    | Sample(e, d) -> Skip, Sample(e, d), Skip
    | Decl(arg, s) -> failwith "no local declarations yet"
    | Seq(s1, s2) -> 
        let sd1, sm1, sq1 = shred_S gamma s1
        let sd2, sm2, sq2 = shred_S gamma s2
        Seq(sd1, sd2) |> skippify, 
        Seq(sm1, sm2) |> skippify, 
        Seq(sq1, sq2) |> skippify
 
    | If(e, s1, s2) -> 
        let sd1, sm1, sq1 = shred_S gamma s1
        let sd2, sm2, sq2 = shred_S gamma s2

        If(e, sd1, sd2) |> skippify,
        If(e, sm1, sm2) |> skippify,
        If(e, sq1, sq2) |> skippify

    | For(arg, lower, upper, s) -> 
        let sd, sm, sq = shred_S gamma s

        For(arg, lower, upper, sd) |> skippify,
        For(arg, lower, upper, sm) |> skippify,
        For(arg, lower, upper, sq) |> skippify

    | Skip -> Skip, Skip, Skip

    | Message _ -> Skip, S, Skip
    | Elim _ -> Skip, S, Skip
    | Generate _ -> Skip, Skip, S