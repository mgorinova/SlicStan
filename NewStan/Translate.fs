module Translate

open SlicStanSyntax
open MiniStanSyntax
open Elaborate


let assigns = Typecheck.assigns

let emptyStanProg = P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)


let join_stan_p (p1: Prog) (p2 :Prog) : Prog =

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


let rec get_type (env: (Type*Ide) list) (x: Ide) : Type = 
    match env with 
    | [] -> failwith (sprintf "no variable %s in program environment" x)
    | (ty, name) :: ns -> if name = x then ty else get_type ns x


let rec to_Stan_statements (S: S) : Statements =
    match S with 
    | SlicStanSyntax.Seq(S1, S2) -> SSeq(to_Stan_statements S1, to_Stan_statements S2)
    | SlicStanSyntax.If(E, S1, S2) -> If(E, to_Stan_statements S1, to_Stan_statements S2)
    | SlicStanSyntax.Assign(a1, a2) -> Let(a1, a2)
    | SlicStanSyntax.Sample(a1, a2) -> Sample(a1, a2) 
    | SlicStanSyntax.Skip -> SNone
    | SlicStanSyntax.Decl _ -> failwith "unexpected in translation"

let translate (C: Context) (S: S) : Prog =

    let gamma = Set.toList C
    let assset = assigns S

    let rec translate_Gamma (gamma: (Type * Ide) list) =
        let allocate_declaration (T: Type, x: Ide) =
            let tau, ell = T
            match ell with
            | Data ->  if Set.contains x assset 
                       then P(DNone, TDBlock(Declr(tau, x),SNone), PNone, TPNone, MBlock(VNone,SNone), GQNone)
                       else P(DBlock(Declr(tau, x)), TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)
            | Model -> if Set.contains x assset 
                       then P(DNone, TDNone, PNone, TPBlock(Declr(tau, x),SNone), MBlock(VNone,SNone), GQNone)
                       else P(DNone, TDNone, PBlock(Declr(tau, x)), TPNone, MBlock(VNone,SNone), GQNone)
            | GenQuant -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(Declr(tau, x),SNone))
            | _ -> failwith "unexpected"

        List.fold (fun p d -> join_stan_p p (allocate_declaration d)) emptyStanProg gamma

    let rec _translate (S:S) : Prog =        
        match S with     
        | SlicStanSyntax.Sample(x, D) -> 
            P(DNone, TDNone, PNone, TPNone, MBlock(VNone, Sample(x,D)), GQNone)

        | SlicStanSyntax.Assign(lhs, E) -> 
            let t, lr = get_type gamma (LValueBaseName lhs) 
            match lr with 
            | Data     -> P(DNone, TDBlock(VNone, Let(lhs,E)), PNone, TPNone, MBlock(VNone,SNone), GQNone)
            | Model    -> P(DNone, TDNone, PNone, TPBlock(VNone, Let(lhs,E)), MBlock(VNone,SNone), GQNone)
            | GenQuant -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(VNone, Let(lhs,E)))
            | _ -> failwith "unexpected error!"
        
        | SlicStanSyntax.Seq(S1, S2) -> 
            let p1 = _translate S1  
            let p2 = _translate S2  
            join_stan_p p1 p2

        | SlicStanSyntax.If(E, S1, S2) ->
            // FIXME: figure out how to do this correctly
            P(DNone, TDNone, PNone, TPBlock(VNone, If(E, to_Stan_statements S1, to_Stan_statements S2)), MBlock(VNone,SNone), GQNone)

        | Skip        -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)
        
        | SlicStanSyntax.Decl _ -> failwith "unexpected"  

    let p = translate_Gamma gamma
    join_stan_p p (_translate S)