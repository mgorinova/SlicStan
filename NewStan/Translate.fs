module Translate

open NewStanSyntax
open MiniStanSyntax
open Types

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




let rec translate (S:S) (env: Dict) (data: string list, modelled: string list) : Prog =
    match S with
    | Data(x) -> 
        let t, _ = env.Item(x)
        P(DBlock(Declr(t,x)), TDNone, PNone, TPNone, MBlock(VNone, SNone), GQNone)
     
    | NewStanSyntax.Sample(x,D) -> 
        if List.contains x data then 
            P(DNone, TDNone, PNone, TPNone, MBlock(VNone, Sample(x,D)), GQNone)

            // TODO: need to figure out how to distinguis the variables defined locally in "model" from other variables
        else
            let t, _ = env.Item(x)
            P(DNone, TDNone, PBlock(Declr(t,x)), TPNone, MBlock(VNone, Sample(x,D)), GQNone)

    | NewStanSyntax.Let(x,E) -> 
        let t, lr = env.Item(x)

        let l = match lr with
                | OptionLevel(list) -> if List.contains GenQuantLevel list then GenQuantLevel
                                       elif List.contains DataLevel list then DataLevel
                                       else ModelLevel                                        
                | level -> level 

        if List.contains x modelled then 
            P(DNone, TDNone, PNone, TPNone, MBlock(Declr(t,x), Let(x,E)), GQNone)
        else
            match l with 
            | DataLevel     -> P(DNone, TDBlock(Declr(t,x), Let(x,E)), PNone, TPNone, MBlock(VNone,SNone), GQNone)
            | ModelLevel    -> P(DNone, TDNone, PNone, TPBlock(Declr(t,x), Let(x,E)), MBlock(VNone,SNone), GQNone)
            | GenQuantLevel -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(Declr(t,x), Let(x,E)))
            | _ -> failwith "unexpected error!"
        

    | Seq(S1,S2) -> 
        let p1 = translate S1 env (data, modelled)
        let p2 = translate S2 env (data, modelled)
        join_stan_p p1 p2

    | Skip -> failwith "PrimDef not implemented"
    | PrimDef(f,[],S) -> failwith "PrimDef not implemented"
    | PrimDef(f,ys,S) -> failwith "PrimDef not implemented"
    | Call(x,[]) -> failwith "Call not implemented"
    | Call(x,Es) -> failwith "Call not implemented"