module Examples

open NewStanSyntax
open Types


let ex_simple = DataDecl(Real, "alpha", Sample("beta",Dist("gamma",[Var"alpha";Const(1.0)])));

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
(*
let ex2 = SofList [Decl((Real, Data), "y1");
                   Assign("alpha",Const(0.1));
                   Assign("beta",Const(0.1));
                   Sample("tau_y",Dist("gamma",[Var"alpha";Var"beta"]));
                   Assign("sigma_y",Prim("pow",[Var"tau_y";Const(-0.5)]));
                   Sample("mu_y",Dist("normal",[Const(0.0);Const(1.0)]));
                   Sample("y1",Dist("normal",[Var"mu_y";Var"sigma_y"]));
                   Assign("variance_y",Prim("pow",[Var"sigma_y";Const(2.0)]))]

let ex_toy = SofList [Decl((Real, Data), "x");
                      Decl((Real, Data),"y");
                      Decl((Real, Data),"z");]

let ex_linear = SofList [Decl((Real, Data), "x1");
                         Decl((Real, Data), "x2");
                         Decl((Real, Data), "x3");
                         Decl((Real, Data), "y1");
                         Decl((Real, Data), "y2");
                         Decl((Real, Data), "y3");
                         Sample("e", Dist("normal",[Const(0.0);Const(10.0)]));
                         Sample("A", Dist("normal",[Const(0.0);Const(1.0)]));
                         Sample("B", Dist("normal",[Const(0.0);Const(1.0)]));
                         Sample("y1", Dist("normal",[Plus(Mul(Var("A"),Var("x1")), Var("B"));Var("e")]));
                         Sample("y2", Dist("normal",[Plus(Mul(Var("A"),Var("x2")), Var("B"));Var("e")]));
                         Sample("y3", Dist("normal",[Plus(Mul(Var("A"),Var("x3")), Var("B"));Var("e")]))]

*)

let S_mynormal = Block( set [((Real, Model), "xr"); ((Real, Model), "x")], SofList[
                          Sample("xr", Dist("normal", [Const(0.0); Const(1.0)]));
                          Assign("x", Plus(Mul(Var "v", Var "xr"), Var "m"))])
let S_main = Block( set [((Real, Model), "y")], Assign("y", ECall("MyNormal", [Const 5.0; Const 2.0])))
let ex_mynormal: NewStanProg = [FunE("MyNormal", [((Real, Data), "m"); ((Real, Data), "v")], S_mynormal, Var("x"))], S_main

/////////////////////////////////////////////

let S_linear =   Block( set [((Real, Model), "alpha"); ((Real, Model), "beta"); ((Real, Model), "var")],
                        SofList[ Sample("alpha", Dist("normal", [Const(0.0); Const(1.0)])); 
                                 Sample("beta", Dist("normal", [Const(0.0); Const(1.0)])); 
                                 Sample("var", Dist("normal", [Const(0.0); Const(1.0)])); 
                                 Sample("y", Dist("normal", [(Plus(Mul(Var("alpha"), Var("x")), Var("beta"))); Var("var")]))])


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
                         SSeq(Let("alpha", Const(0.1)), 
                              Let("beta", Const(0.1)))), 
                 PBlock(VSeq(Declr(Real, "tau_y"),Declr(Real, "mu_y"))), 
                 TPBlock(Declr(Real, "sigma_y"),Let("sigma_y", Prim("pow",[Var("tau_y");Const(-0.5)]))), 
                 MBlock(VNone, SSeq(SSeq(Sample("tau_y", Dist("gamma",[Var("alpha"); Var("beta")])),
                                         Sample("mu_y", Dist("normal",[Const(0.0); Const(1.0)]))),
                                         Sample("y1", Dist("normal",[Var("mu_y"); Var("sigma_y")])))),
                 GQBlock(Declr(Real, "variance_y"),Let("variance_y", Prim("pow", [Var("sigma_y");Const(2.0)]))))