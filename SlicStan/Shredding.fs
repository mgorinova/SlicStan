module Shredding

open SlicStanSyntax
open Typecheck
open Elaborate

let rec skippify S = 
    match S with
    | Seq(Skip, Skip) -> Skip
    | If(_, Skip, Skip) -> Skip
    | For(_, _, _, Skip) -> Skip
    | Elim(_, _, Skip) -> Skip
    //| Elim(arg, _, s) -> if Set.contains (snd arg) (Util.reads s) then S else s
    | _ -> S

let statement_level gamma S =  
    let one = Util.reads S |> Set.toList                       
    let two = one |> List.map (fun x -> Map.find x gamma |> snd) |> Lub |> Typecheck.simplify_level
    // FIXME: it's more complicated than this line below... the good news is that this is only an
    // extra optimisation (is it just that?) so things still work without it. 
    // Examples that can be made faster with an optimisation of some sort are discrete_reverse_tree or discrete_tree (? one of the two i thin)
    //if Typecheck.local_blocks && List.contains (Typecheck.dv) one |> not then GenQuant else two
    two

let shred_according_to_statement_level gamma S =
    let st_level = statement_level gamma S
    match st_level with
        | Data -> S, Skip, Skip
        | Model -> Skip, S, Skip
        | GenQuant -> Skip, Skip, S
        | _ -> failwith "unexpected!"

let rec shred_S (gamma: Gamma) (S: S) : (S * S * S) =
    match S with 
    | Assign(lhs, e) -> 
        let base_name = LValueBaseName lhs
        match (Map.gammaItemTypeLevel base_name gamma) with
        | Data -> Assign(lhs, e), Skip, Skip
        | Model -> Skip, Assign(lhs, e), Skip 
        | GenQuant -> Skip, Skip, Assign(lhs, e)
        | _ -> failwith "something went terribly worng"

    | Sample _ -> 
        if toplevel then Skip, S, Skip
        else shred_according_to_statement_level gamma S        

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
        let ell_e = Typecheck.synth_E (Buildins) gamma e
                   |> fst |> snd
                   |> Typecheck.simplify_type

        match ell_e with 
        | Data ->
            If(e, sd1, sd2) |> skippify,
            If(e, sm1, sm2) |> skippify,
            If(e, sq1, sq2) |> skippify
        | Model -> 
            Skip,
            If(e, Seq(sd1,sm1), Seq(sd2,sm2)) |> skippify,
            If(e, sq1, sq2) |> skippify
        | GenQuant ->
            Skip, Skip, If(e, s1, s2)

    | For(arg, lower, upper, s) -> 
        let sd, sm, sq = shred_S (Map.add (snd arg) (fst arg) gamma) s

        For(arg, lower, upper, sd) |> skippify,
        For(arg, lower, upper, sm) |> skippify,
        For(arg, lower, upper, sq) |> skippify

    | Skip -> Skip, Skip, Skip 

    | Message (arg, _, _) -> 
        let gamma' = Map.add (snd arg) (fst arg) gamma
        //let sd, sm, sq = shred_according_to_statement_level (Map.add (snd arg) (fst arg) gamma') S 
        //skippify sd, skippify sm, skippify sq
        shred_according_to_statement_level (Map.add (snd arg) (fst arg) gamma') S 

    | Elim (arg, m, s') -> 
        let gamma' = Map.add (snd arg) (Int, Model) gamma
        let sd, sm, sq = shred_S gamma' s'

        //Elim(arg, m, sd) |> skippify,
        //Elim(arg, m, sm) |> skippify,
        //Elim(arg, m, sq) |> skippify
        
        // TODO: try deriving actual rule based on for loops?
        
        shred_according_to_statement_level (Map.add (snd arg) (fst arg) gamma) S
    | Generate _ -> Skip, Skip, S