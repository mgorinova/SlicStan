module Translate

open NewStanSyntax
open MiniStanSyntax
open Elaborate
//open Types

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



let translate (S:S) (data: string list, modelled: string list) : Prog =

    let rec to_declarations vars =
        match vars with 
        | [] -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)
        | ((t,l), var) :: vs ->
            if List.contains var data then
                let p = P(DBlock(Declr(t, var)), TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)
                join_stan_p p (to_declarations vs)
            else if List.contains var modelled && l = Model then
                let p = P(DNone, TDNone, PBlock(Declr(t, var)), TPNone, MBlock(VNone,SNone), GQNone)
                join_stan_p p (to_declarations vs)
            else if List.contains var modelled && l = Data then
                let p = P(DNone, TDBlock(Declr(t, var), SNone), PNone, TPNone, MBlock(VNone,SNone), GQNone)
                join_stan_p p (to_declarations vs)
            else match l with
            | Data ->     let p = P(DNone, TDBlock(Declr(t, var), SNone), PNone, TPNone, MBlock(VNone,SNone), GQNone)
                          join_stan_p p (to_declarations vs)
            | Model ->    let p = P(DNone, TDNone, PNone, TPBlock(Declr(t, var), SNone), MBlock(VNone,SNone), GQNone)
                          join_stan_p p (to_declarations vs)
            | Local ->    let p = P(DNone, TDNone, PNone, TPNone, MBlock(Declr(t, var),SNone), GQNone)
                          join_stan_p p (to_declarations vs)
            | GenQuant -> let p = P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(Declr(t,var), SNone))
                          join_stan_p p (to_declarations vs)
            | _ -> failwith "unexpected in translate init: to_declarations"

    let rec _translate (S:S) (env: Env) : Prog =

        let env_list = Set.toList env
        match S with     
        | NewStanSyntax.Block(var, statements) -> 
            let p = to_declarations [var]
            let enew = Set.add var env
            join_stan_p p (_translate statements enew)

        | NewStanSyntax.Sample(x,D) -> 
            P(DNone, TDNone, PNone, TPNone, MBlock(VNone, Sample(x,D)), GQNone)

        | NewStanSyntax.Assign(x,E) -> 
            let t, lr = get_type env_list x 
            match lr with 
            | Data     -> P(DNone, TDBlock(VNone, Let(x,E)), PNone, TPNone, MBlock(VNone,SNone), GQNone)
            | Model    -> P(DNone, TDNone, PNone, TPBlock(VNone, Let(x,E)), MBlock(VNone,SNone), GQNone)
            | Local    -> P(DNone, TDNone, PNone, TPBlock(VNone, SNone), MBlock(VNone, Let(x,E)), GQNone)
            | GenQuant -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(VNone, Let(x,E)))
            | _ -> failwith "unexpected error!"
        

        | NewStanSyntax.Seq(S1,S2) -> 
            let p1 = _translate S1 env 
            let p2 = _translate S2 env 
            join_stan_p p1 p2

        | Skip        -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)
        | DataDecl(_,_,s) -> _translate s env


    _translate S Set.empty