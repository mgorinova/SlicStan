module Examples

open NewStanSyntax
open Types


let rec BlockOfList (env, s) = 
    match env with 
    | [] -> s
    | x::xs -> Block(x, BlockOfList (xs, s))


let ex_simple: NewStanProg = [], DataDecl(Real, "alpha", Sample("beta",Dist("gamma",[Var"alpha";Const(1.0)])));

let ex_arrays: NewStanProg = [], (BlockOfList( [((Real, Data), "x"); ((Real, Data), "y")], 
                                               SofList [Assign(I("x"), Arr [Const(0.0); Const(2.0); Const(1.5)]);
                                                        Assign(I("y"), ArrElExp(Var("x"), Const(2.0)))]))


/////////////////////////////////////////////

let multinormal = BlockOfList( [((Array(Real,2), Model), "xr"); ((Array(Array(Real, 2), 2), Model), "L"); ((Array(Real,2), Model), "x")], SofList[
                                  Assign(I("L"), Prim("cholesky_decompose", [Var("Sigma")]))
                                  Sample("xr", Dist("normal", [Const(0.0); Const(1.0)]));
                                  Assign(I("x"), Plus(Mul(Var "L", Var "xr"), Var "mu"))])

let defs_multinormal = [FunE("MyMultiNormal", [((Array(Real,2), Data), "mu"); ((Array(Array(Real, 2), 2), Data), "Sigma")], multinormal, (Real, Model), Var("x"))]

let mu = Arr [Const(2.0); Const(3.4)]
let Sigma = Arr [(Arr [Const(0.3); Const(0.1)]);(Arr [Const(0.1); Const(2.0)])]

let main_multinormal = BlockOfList( [((Real, Model), "x")], Assign(I("x"), ECall("MyMultiNormal", [mu; Sigma])))

let ex_multinormal:NewStanProg = defs_multinormal, main_multinormal
                                          
// Example 2: from the Stan manual
(*  
    alpha = 0.1; 
    beta = 0.1; 
    tau_y ~ gamma(alpha,beta); 
    sigma_y = pow(tau_y,-0.5); 
    mu_y ~ normal(0,1); 
    y1 ~ normal(mu_y,sigma_y); 
    variance_y = *(sigma_y,sigma_y);
*)

let ex_simple_normal: NewStanProg = [], BlockOfList( [((Real, Model), "y")], Sample("y", Dist("normal", [Const 5.0; Const 2.0])))


let S_mynormal = BlockOfList( [((Real, Model), "xr"); ((Real, Model), "x")], SofList[
                               Sample("xr", Dist("normal", [Const(0.0); Const(1.0)]));
                               Assign(I("x"), Plus(Mul(Var "v", Var "xr"), Var "m"))])
let S_main = BlockOfList( [((Real, Model), "y")], Assign(I("y"), ECall("MyNormal", [Const 5.0; Const 2.0])))
let ex_mynormal: NewStanProg = [FunE("MyNormal", [((Real, Data), "m"); ((Real, Data), "v")], S_mynormal, (Real, Model), Var("x"))], S_main

/////////////////////////////////////////////

let S_mynormal_clash = BlockOfList( [((Real, Model), "xr"); ((Real, Model), "x")], SofList[
                                     Sample("xr", Dist("normal", [Const(0.0); Const(1.0)]));
                                     Assign(I("x"), Plus(Mul(Var "v", Var "xr"), Var "m"))])
let S_main_clash = BlockOfList( [((Real, Model), "x")], Assign(I("x"), ECall("MyNormal", [Const 5.0; Const 2.0])))
let ex_mynormal_clash: NewStanProg = [FunE("MyNormal", [((Real, Data), "m"); ((Real, Data), "v")], S_mynormal_clash, (Real, Model), Var("x"))], S_main_clash

/////////////////////////////////////////////

let S_mynormal_clash2 = BlockOfList( [((Real, Model), "xr"); ((Real, Model), "x")], SofList[
                                      Sample("xr", Dist("normal", [Const(0.0); Const(1.0)]));
                                      Assign(I("x"), Plus(Mul(Var "v", Var "xr"), Var "m"))])
let S_main_clash2 = BlockOfList( [((Real, Model), "x"); ((Real, Model), "mu")], 
                                 SofList([Sample("mu", Dist("normal", [Const(0.0); Const(1.0)])); 
                                          Assign(I("x"), ECall("MyNormal", [Plus(Var("mu"), Const 5.0); Const 2.0]))]) )
let ex_mynormal_clash2: NewStanProg = [FunE("MyNormal", [((Real, Model), "m"); ((Real, Data), "v")], S_mynormal_clash2, (Real, Model), Var("x"))], S_main_clash2


/////////////////////////////////////////////

let S_mynormal_clash3 = BlockOfList( [((Real, Model), "xr"); ((Real, Model), "x")], SofList[
                                      Sample("xr", Dist("normal", [Const(0.0); Const(1.0)]));
                                      Assign(I("x"), Plus(Mul(Var "v", Var "xr"), Var "m"))])
let S_main_clash3 = BlockOfList( [((Real, Model), "x"); ((Real, Model), "mu"); ((Real, Model), "y")], 
                                 SofList([Sample("mu", Dist("normal", [Const(0.0); Const(1.0)])); 
                                          Assign(I("x"), ECall("MyNormal", [Plus(Var("mu"), Const 5.0); Const 2.0]));
                                          Sample("y", Dist("normal", [ECall("MyNormal", [Var("x"); Const 2.0]); Const(1.0)]));]) )
let ex_mynormal_clash3: NewStanProg = [FunE("MyNormal", [((Real, Model), "m"); ((Real, Data), "v")], S_mynormal_clash3, (Real, Model), Var("x"))], S_main_clash3

/////////////////////////////////////////////

let S_linear =   BlockOfList( [((Real, Model), "alpha"); ((Real, Model), "beta"); ((Real, Model), "e")],
                        SofList[ Sample("alpha", Dist("normal", [Const(0.0); Const(1.0)])); 
                                 Sample("beta", Dist("normal", [Const(0.0); Const(1.0)])); 
                                 Sample("e", Dist("normal", [Const(0.0); Const(1.0)])); 
                                 Sample("y", Dist("normal", [(Plus(Mul(Var("alpha"), Var("x")), Var("beta"))); Var("e")]))])


let fundefs_linear = FunV("LR", [((Real, Data), "x"); ((Real, Data), "y")], S_linear)
let main_linear = DataDecl(Real, "input_x", DataDecl(Real, "input_y", VCall("LR", [Var("input_x"); Var("input_y")])))

// block : (Type * Ide) list * S list -> S

let ex_linear_funcs = [fundefs_linear], main_linear

/////////////////////////////////////////////

(******************** Stan Eaxmples *********************)


let Declr = MiniStanSyntax.Declr
let Let = MiniStanSyntax.Let
let VSeq = MiniStanSyntax.VSeq
let SSeq = MiniStanSyntax.SSeq
let Sample = MiniStanSyntax.Sample
let P = MiniStanSyntax.P
let DBlock = MiniStanSyntax.DBlock
let TDBlock = MiniStanSyntax.TDBlock
let PBlock = MiniStanSyntax.PBlock
let TPBlock = MiniStanSyntax.TPBlock
let MBlock = MiniStanSyntax.MBlock
let GQBlock = MiniStanSyntax.GQBlock
let VNone = MiniStanSyntax.VNone 

let example2 = P(DBlock(Declr(Real, "y1")), 
                 TDBlock(VSeq(Declr(Real, "alpha"),
                              Declr(Real, "beta")), 
                         SSeq(Let(I("alpha"), Const(0.1)), 
                              Let(I("beta"), Const(0.1)))), 
                 PBlock(VSeq(Declr(Real, "tau_y"),Declr(Real, "mu_y"))), 
                 TPBlock(Declr(Real, "sigma_y"),Let(I("sigma_y"), Prim("pow",[Var("tau_y");Const(-0.5)]))), 
                 MBlock(VNone, SSeq(SSeq(Sample("tau_y", Dist("gamma",[Var("alpha"); Var("beta")])),
                                         Sample("mu_y", Dist("normal",[Const(0.0); Const(1.0)]))),
                                         Sample("y1", Dist("normal",[Var("mu_y"); Var("sigma_y")])))),
                 GQBlock(Declr(Real, "variance_y"),Let(I("variance_y"), Prim("pow", [Var("sigma_y");Const(2.0)]))))