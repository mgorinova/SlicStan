module Transformation

open SlicStanSyntax
open MiniStanSyntax
open Elaborate



let assigns = Typecheck.assigns

let emptyStanProg = P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)

let join_stan_p (p1: MiniStanProg) (p2 :MiniStanProg) : MiniStanProg =

    let join_block (block1 : VarDecls * Statements) (block2 : VarDecls * Statements) : VarDecls * Statements =        
        let vs1, ss1 = block1
        let vs2, ss2 = block2
        VSeq(vs1, vs2), SSeq(ss1, ss2)
        
            
    let d1, td1, p1, tp1, m1, gq1 = match p1 with P(d1, td1, p1, tp1, m1, gq1) -> (d1, td1, p1, tp1, m1, gq1)
    let d2, td2, p2, tp2, m2, gq2 = match p2 with P(d2, td2, p2, tp2, m2, gq2) -> (d2, td2, p2, tp2, m2, gq2)

    let d = match d1, d2 with 
            | DBlock(vs1), DBlock(vs2) -> 
                let vs, _ = join_block (vs1, SNone) (vs2, SNone)
                DBlock(vs)
            | db1, DNone -> db1
            | DNone, db2 -> db2

    let td = match td1, td2 with 
             | TDBlock(vs1, ss1), TDBlock(vs2, ss2) -> 
                let vs, ss = join_block (vs1, ss1) (vs2, ss2)
                TDBlock(vs, ss)
             | db1, TDNone -> db1
             | TDNone, db2 -> db2

    let p = match p1, p2 with 
            | PBlock(vs1), PBlock(vs2) -> 
                let vs, _ = join_block (vs1, SNone) (vs2, SNone)
                PBlock(vs)
            | db1, PNone -> db1
            | PNone, db2 -> db2

    let tp = match tp1, tp2 with 
             | TPBlock(vs1, ss1), TPBlock(vs2, ss2) -> 
                let vs, ss = join_block (vs1, ss1) (vs2, ss2)
                TPBlock(vs, ss)
             | db1, TPNone -> db1
             | TPNone, db2 -> db2

    let m = match m1, m2 with 
            | MBlock(vs1, ss1), MBlock(vs2, ss2) -> 
            let vs, ss = join_block (vs1, ss1) (vs2, ss2)
            MBlock(vs, ss)

    let gq = match gq1, gq2 with 
             | GQBlock(vs1, ss1), GQBlock(vs2, ss2) -> 
                let vs, ss = join_block (vs1, ss1) (vs2, ss2)
                GQBlock(vs, ss)
             | db1, GQNone -> db1
             | GQNone, db2 -> db2

    P (d, td, p, tp, m, gq)


let rec to_Stan_statements (S: S) : Statements =
    match S with 
    | SlicStanSyntax.Seq(S1, S2) -> SSeq(to_Stan_statements S1, to_Stan_statements S2)
    | SlicStanSyntax.If(E, S1, S2) -> If(E, to_Stan_statements S1, to_Stan_statements S2)
    | SlicStanSyntax.Assign(a1, a2) -> Let(a1, a2)
    | SlicStanSyntax.Sample(a1, a2) -> Sample(a1, a2) 
    | SlicStanSyntax.Skip -> SNone
    | SlicStanSyntax.Decl _ -> failwith "unexpected in translation"


let is_target lhs =
    match lhs with 
    | I(x) -> x = "target"
    | _ -> false

let rec has_target S = 
    match S with 
    | Assign(lhs, e) -> is_target lhs
    | SlicStanSyntax.Sample _ -> true
    | Seq(s1, s2) -> has_target s1 || has_target s2
    | Decl(_, s) -> has_target s
    | SlicStanSyntax.If(e, s1, s2) -> has_target s1 || has_target s2
    | Skip -> false


let transform_gamma (W: Set<Ide>) (gamma : Context) : MiniStanProg =
    let transform_single_decl (arg: Arg) =
        let (tp, tl), x = arg 
        match tl with
        | Data ->
            if Set.contains x W 
            then P(DNone, TDBlock(Declr(tp, x),SNone), PNone, TPNone, MBlock(VNone,SNone), GQNone)
            else P(DBlock(Declr(tp, x)), TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone) 

        | Model -> 
            if Set.contains x W 
            then P(DNone, TDNone, PNone, TPBlock(Declr(tp, x),SNone), MBlock(VNone,SNone), GQNone)
            else P(DNone, TDNone, PBlock(Declr(tp, x)), TPNone, MBlock(VNone,SNone), GQNone)

        | GenQuant -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(Declr(tp, x),SNone))
        | _ -> failwith "something has gone very wrong"

    Set.fold (fun current_prog arg -> transform_single_decl arg |> join_stan_p current_prog) emptyStanProg gamma


let transform_data (S: S) : MiniStanProg =
    P(DNone, TDBlock(VNone, to_Stan_statements S), PNone, TPNone, MBlock(VNone,SNone), GQNone)


let rec transform_model (S: S) : MiniStanProg =
    match S with 
    | Assign(lhs, e) -> 
        if is_target lhs 
        then P(DNone, TDNone, PNone, TPNone, MBlock(VNone, Let(lhs, e)), GQNone)  
        else P(DNone, TDNone, PNone, TPBlock(VNone, Let(lhs, e)), MBlock(VNone, SNone), GQNone)
    | SlicStanSyntax.Sample(e, d) -> 
        P(DNone, TDNone, PNone, TPNone, MBlock(VNone, Sample(e, d)), GQNone)  
    | SlicStanSyntax.If(e, s1, s2) -> 
        if has_target s1 || has_target s2 
        then P(DNone, TDNone, PNone, TPNone, MBlock(VNone, to_Stan_statements S), GQNone)  
        else P(DNone, TDNone, PNone, TPBlock(VNone, to_Stan_statements S), MBlock(VNone, SNone), GQNone)
    | Decl _ -> failwith "unexpected"
    | Seq(s1, s2) -> 
        transform_model s1 |> 
        join_stan_p (transform_model s2)
    | Skip -> emptyStanProg


let transform_quant (S: S) : MiniStanProg =
    P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(VNone, to_Stan_statements S))


let transform (gamma : Context) (Sd : S, Sm : S , Sq : S ) : MiniStanProg = 

    let W = assigns Sd 
         |> Set.union (assigns Sd)
         |> Set.union (assigns Sm)
         |> Set.union (assigns Sq)

    transform_gamma W gamma 
    |> join_stan_p (transform_data Sd)  
    |> join_stan_p (transform_model Sm)
    |> join_stan_p (transform_quant Sq)


