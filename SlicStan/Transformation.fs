module Transformation

open SlicStanSyntax
open MiniStanSyntax
open Elaborate
open Util
open Factorgraph


let mutable avoid_ides : Ide list = []


let rec mentions (S : Statements) : Ide list = 
    match S with 
    | Let(lhs, e) -> reads e |> List.append (lhs_to_exp lhs |> reads)
    | Sample(lhs, d) -> read_dist d |> Set.toList |> List.append (lhs_to_exp lhs |> reads) 
    | Factor e -> reads e
    | PlusEq(lhs, e) -> reads e |> List.append (lhs_to_exp lhs |> reads)
    | If(e, s1, s2) -> reads e |> List.append (mentions s1) |> List.append (mentions s2)
    | For(x, l, u, s') -> x :: (mentions s') |> List.append (read_size l) |> List.append (read_size u)
    | SSeq (s1, s2) -> mentions s1 |> List.append (mentions s2)
    | LocalDecl(t, x, s') -> x :: (mentions s') 
    | SNone -> []


let fresh_acc_ide (S : Statements) : Ide =
    let rec generate_ide x n = 
        let cur = sprintf "%s%A" x n
        if List.contains cur avoid_ides || List.contains cur (mentions S)
        then generate_ide x (n+1)
        else cur

    generate_ide "acc" 0


let assigns = Typecheck.assigns

let emptyStanProg = P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQNone)


let rec rename_Stan_Exp (oldn : Ide) (newn : Ide) (exp: Exp) : Exp = 
    match exp with
    | Var x -> if x = oldn then Var newn else Var x
    | Const _ -> exp
    | Arr es -> List.map ( rename_Stan_Exp oldn newn ) es |> Arr
    | ArrElExp (e1, e2) -> ArrElExp (rename_Stan_Exp oldn newn e1, rename_Stan_Exp oldn newn e2)
    | Plus (e1, e2) -> Plus (rename_Stan_Exp oldn newn e1, rename_Stan_Exp oldn newn e2)
    | Mul (e1, e2) -> Mul (rename_Stan_Exp oldn newn e1, rename_Stan_Exp oldn newn e2)
    | Prim(name, es) -> Prim (name, List.map ( rename_Stan_Exp oldn newn ) es)

let rec rename_Stan_LValues (oldn : Ide) (newn : Ide) (lhs: LValue) : LValue = 
    match lhs with 
    | I x -> if x = oldn then I newn else I x
    | A (lhs', e) -> A (rename_Stan_LValues oldn newn lhs', rename_Stan_Exp oldn newn e)

let rename_Stan_Dist (oldn : Ide) (newn : Ide) (dist: Dist) : Dist = 
    match dist with 
    | Dist(name, es) -> Dist(name, List.map ( rename_Stan_Exp oldn newn ) es)

let rec rename_Stan_Statements (oldn : Ide) (newn : Ide) (S : Statements) : Statements = 
    match S with
    | Let(lhs, e) -> Let(rename_Stan_LValues oldn newn lhs, rename_Stan_Exp oldn newn e)
    | Sample(lhs, d) -> Sample(rename_Stan_LValues oldn newn lhs, rename_Stan_Dist oldn newn d)
    | Factor e -> Factor (rename_Stan_Exp oldn newn e)
    | PlusEq(lhs, e) -> PlusEq(rename_Stan_LValues oldn newn lhs, rename_Stan_Exp oldn newn e)
    | If(e, s1, s2) -> If(rename_Stan_Exp oldn newn e, rename_Stan_Statements oldn newn s1, rename_Stan_Statements oldn newn s2)
    | For(x, l, u, s') -> 
        if x = oldn then For(newn, l, u, rename_Stan_Statements oldn newn s') 
        else For(x, l, u, rename_Stan_Statements oldn newn s') 
    | SSeq (s1, s2) -> SSeq(rename_Stan_Statements oldn newn s1, rename_Stan_Statements oldn newn s2)
    | LocalDecl(t, x, s') -> LocalDecl(t, x, rename_Stan_Statements oldn newn s')
    | SNone -> SNone
        

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

let get_support (t: TypePrim) : ArrSize =
    match t with
    | Constrained(Int, K) -> K
    | _ -> failwith "unexpected"


let to_lpdf (dist: Dist) (x : LValue) : Exp = 
    match dist with
    | Dist(name, es) -> 
        if name = "categorical" || name = "bernoulli" 
        then Prim (name + "_lpmf", (( lhs_to_exp x ) :: es))
        else Prim (name + "_lpdf", (( lhs_to_exp x ) :: es))
   
let to_rng (dist: Dist) : Exp = 
    match dist with
    | Dist(name, es) -> Prim (name + "_rng", es)

let rec target_in (acc : LValue) (S : Statements) : Statements = 
    match S with 
    | Sample(lhs, dist) -> PlusEq(acc, to_lpdf dist lhs)
    | Factor(exp) -> PlusEq(acc, exp)
    | PlusEq(I "target", exp) -> PlusEq(acc, exp)
    | SSeq(s1, s2) -> SSeq(target_in acc s1, target_in acc s2)
    | If(e, s1, s2) -> If(e, target_in acc s1, target_in acc s2)
    | For(x, l, u, s) -> For(x, l, u, target_in acc s)
    | LocalDecl(t, x, s) -> LocalDecl(t, x, target_in acc s)
    | _ -> S


let rec to_Stan_statements (S: S) : Statements =
    match S with 
    | SlicStanSyntax.Seq(S1, S2) -> SSeq(to_Stan_statements S1, to_Stan_statements S2)
    | SlicStanSyntax.If(E, S1, S2) -> If(E, to_Stan_statements S1, to_Stan_statements S2)
    | SlicStanSyntax.For(E, l, u, S') -> For(snd E, l, u, to_Stan_statements S')
    | SlicStanSyntax.Assign(a1, a2) -> Let(a1, a2)
    | SlicStanSyntax.Sample(a1, a2) -> Sample(a1, a2) 
    | SlicStanSyntax.Factor(e) -> PlusEq(I("target"), e)
    | SlicStanSyntax.Skip -> SNone
    | SlicStanSyntax.Decl _ -> failwith "unexpected in translation"
    | SlicStanSyntax.Message((T, x), args, s) ->
        
        if List.length args > 1 then      
            failwith "Translation of message to Stan not yet implemented"

        elif List.length args = 1 then 
            let a = List.head args            
            //let support_arr_size = get_support(fst T) 
            //let support = match support_arr_size with N(n) -> Const(float n) | SizeVar(str) -> Var(str)
            let support_arr_size = N(2)
            let support = Const(2.0) // FIXME: not the right size
            let def = Let(I(x), Prim("rep_vector", [Const(0.0); support]))
            let loop = For(a, N(1), support_arr_size, to_Stan_statements s |> target_in (A(I(x), Var(a))))        
            SSeq(def, loop)

        else 
            let def = Let(I(x), Const(0.0))
            let loop = to_Stan_statements s |> target_in (I(x))  
            SSeq(def, loop)

        (*let support_arr_size = get_support(fst T)
        let support = match support_arr_size with N(n) -> Const(float n) | SizeVar(str) -> Var(str)
        let def = Let(I(message), Prim("rep_vector", [Const(0.0); support]))
        let loop = For(x, N(1), support_arr_size, to_Stan_statements s |> target_in (A(I(message), Var(x))))        
        SSeq(def, loop)*)
    
    | SlicStanSyntax.Elim((T, x), s) -> 
        let support_arr_size = get_support(fst T)
        let support = match support_arr_size with N(n) -> Const(float n) | SizeVar(str) -> Var(str)
        
        let statement = to_Stan_statements s

        let accname = fresh_acc_ide statement
        let def = Let(I accname, Prim("rep_vector", [Const(0.0); support])) 
        let inner = statement |> target_in (A( I accname, Var x ))
        let loop = For(x, N(1), support_arr_size, inner)
                    //SSeq(inner, PlusEq( A( I accname, Var x ), ArrElExp(Var message, Var x) )) )
                 |> rename_Stan_Statements x (x + "_val") 
        let sum = Factor( Prim("log_sum_exp", [Var accname]) )
        LocalDecl(Vector support_arr_size, accname, SSeq ( (SSeq(def, loop)), sum ))

    | SlicStanSyntax.Generate((T, x), s) ->
        let support_arr_size = get_support(fst T)
        let support = match support_arr_size with N(n) -> Const(float n) | SizeVar(str) -> Var(str)
        
        let statement = to_Stan_statements s

        let accname = fresh_acc_ide statement
        let def = Let(I accname, Prim("rep_vector", [Const(0.0); support])) 
        let inner = statement |> target_in (A( I accname, Var x ))
        let loop = For(x, N(1), support_arr_size, inner)
                    //SSeq(inner, PlusEq( A( I accname, Var x ), ArrElExp(Var message, Var x) )) )
                |> rename_Stan_Statements x (x + "_val") 
        let sample = Let(I x, Prim("categorical_logit_rng", [Var accname]) )
        LocalDecl(Vector support_arr_size, accname, SSeq ( (SSeq(def, loop)), sample ))


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
    | SlicStanSyntax.If(_, s1, s2) -> has_target s1 || has_target s2
    | SlicStanSyntax.For(_, _, _, s) -> has_target s
    | Skip -> false


let transform_gamma (W: Set<Ide>) (gamma : Gamma) : MiniStanProg =
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

    Map.toList gamma
    |> List.map (fun (x, y) -> (y, x))
    |> List.fold (fun current_prog arg -> transform_single_decl arg |> join_stan_p current_prog) emptyStanProg 


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
    | SlicStanSyntax.Factor(e) ->
        P(DNone, TDNone, PNone, TPNone, MBlock(VNone, PlusEq(I("target"), e)), GQNone) 
    | SlicStanSyntax.If(e, s1, s2) -> 
        if has_target s1 || has_target s2 
        then P(DNone, TDNone, PNone, TPNone, MBlock(VNone, to_Stan_statements S), GQNone)  
        else P(DNone, TDNone, PNone, TPBlock(VNone, to_Stan_statements S), MBlock(VNone, SNone), GQNone)
    | SlicStanSyntax.For(arg, lower, upper, s) -> 
        if has_target s 
        then P(DNone, TDNone, PNone, TPNone, MBlock(VNone, to_Stan_statements S), GQNone)  
        else P(DNone, TDNone, PNone, TPBlock(VNone, to_Stan_statements S), MBlock(VNone, SNone), GQNone) 
    | Decl _ -> failwith "unexpected"
    | Seq(s1, s2) -> 
        transform_model s2 |> 
        join_stan_p (transform_model s1)
    | Skip -> emptyStanProg
    | SlicStanSyntax.Message _ -> 
        P(DNone, TDNone, PNone, TPBlock(VNone, to_Stan_statements S), MBlock(VNone, SNone), GQNone)

    | SlicStanSyntax.Elim((T, x), s) -> 
        let support_arr_size = get_support(fst T)
        let support = match support_arr_size with N(n) -> Const(float n) | SizeVar(str) -> Var(str)

        let statement = to_Stan_statements s

        let accname = fresh_acc_ide statement
        let def =  Let(I accname, Prim("rep_vector", [Const(0.0); support]))
        let inner = statement |> target_in (A( I accname, Var x ))
        let loop = For(x, N(1), support_arr_size, inner)
                    //SSeq(inner, PlusEq( A( I accname, Var x ), ArrElExp(Var message, Var x) )) )
                |> rename_Stan_Statements x (x + "_val") 
        let sum = Factor( Prim("log_sum_exp", [Var accname]) )

        P(DNone, TDNone, PNone, TPNone, 
          MBlock(VNone, LocalDecl(Vector support_arr_size, accname, SSeq ( (SSeq(def, loop)), sum ))), GQNone)    

let rec transform_quant (S: S) : MiniStanProg =
    match S with 
    | SlicStanSyntax.Sample(lhs, d) -> 
        P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(VNone, Let(lhs, to_rng d)))
    | SlicStanSyntax.Seq(s1, s2) -> 
        transform_quant s2  |> join_stan_p (transform_quant s1)
    | _ -> P(DNone, TDNone, PNone, TPNone, MBlock(VNone,SNone), GQBlock(VNone, to_Stan_statements S))

    

let transform (gamma : Gamma) (Sd : S, Sm : S , Sq : S ) : MiniStanProg = 

    let W = assigns Sd 
         |> Set.union (assigns Sd)
         |> Set.union (assigns Sm)
         |> Set.union (assigns Sq)

    avoid_ides <- gamma
                |> Map.toList
                |> List.map fst

    transform_gamma W gamma 
    |> join_stan_p (transform_data Sd)  
    |> join_stan_p (transform_model Sm)
    |> join_stan_p (transform_quant Sq)


