module Examples

open NewStanSyntax
open Types

// Example 1
// data y; 
// data w;
// x ~ normal (0, 1);  // checker will record that x is a parameter
// y ~ normal (x, 1);  // checker will record that y is a modelled input (and implicitly that w is an unmodelled input)
// def f(x) { z ~ normal(x,1); return normal(z,1); }
// w ~ f(y)

// NB: there might be better ways of using functions, e.g.
// def f(x) { z ~ normal(0, 1); x ~ normal(z, 1); } 

let ex = S_of_list [Data("y"); Data("w");
                    Sample("x", Dist("normal",[Const 0.0; Const 1.0]));
                    Sample("y", Dist("normal",[Var "x"; Const 1.0]));
                    PrimDef("f",["x"],Sample("z", Dist("normal",[Const 0.0; Const 1.0])));
                    Call("f",[Var "y"])]



let ex_simple = S_of_list [Data("alpha");
                           Sample("beta",Dist("gamma",[Var"alpha";Const(1.0)]))];

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

let ex2 = S_of_list [Data("y1");
                     Let("alpha",Const(0.1));
                     Let("beta",Const(0.1));
                     Sample("tau_y",Dist("gamma",[Var"alpha";Var"beta"]));
                     Let("sigma_y",Prim("pow",[Var"tau_y";Const(-0.5)]));
                     Sample("mu_y",Dist("normal",[Const(0.0);Const(1.0)]));
                     Sample("y1",Dist("normal",[Var"mu_y";Var"sigma_y"]));
                     Let("variance_y",Prim("pow",[Var"sigma_y";Const(2.0)]))]

let ex_toy = S_of_list [Data("x");
                        Data("y");
                        Data("z");]

let ex_linear = S_of_list [Data("x1");
                           Data("x2");
                           Data("x3");
                           Data("y1");
                           Data("y2");
                           Data("y3");
                           Sample("e", Dist("normal",[Const(0.0);Const(10.0)]));
                           Sample("A", Dist("normal",[Const(0.0);Const(1.0)]));
                           Sample("B", Dist("normal",[Const(0.0);Const(1.0)]));
                           Sample("y1", Dist("normal",[Plus(Mul(Var("A"),Var("x1")), Var("B"));Var("e")]));
                           Sample("y2", Dist("normal",[Plus(Mul(Var("A"),Var("x2")), Var("B"));Var("e")]));
                           Sample("y3", Dist("normal",[Plus(Mul(Var("A"),Var("x3")), Var("B"));Var("e")]))]


(* '''
data{
    real kcal1;
    real kcal2;
    real kcal3;
    real neocortex1;
    real neocortex2;
    real neocortex3;
    real logmass1;
    real logmass2;
    real logmass3;
}
parameters{
    real alpha;
    real sigma;
    real bN;
    real bM;
    real mu_nc;
    real sigma_nc;
}
model{
    real mu1;
    real mu2;
    real mu3;
    
    real nc_merged1;
    real nc_merged2;
    real nc_merged3;
    
    alpha ~ normal(0,10);
    bN ~ normal(0,10);
    bM ~ normal(0,10);
    mu_nc ~ normal(0.5,1);
    sigma ~ cauchy(0,1);
    sigma_nc ~ cauchy(0,1);

    nc_merged1 = neocortex1;
    nc_merged2 = neocortex2;
    nc_merged3 = neocortex3;
    
    nc_merged1 ~ normal( mu_nc , sigma_nc );
    nc_merged2 ~ normal( mu_nc , sigma_nc );
    nc_merged3 ~ normal( mu_nc , sigma_nc );
    
    mu1 = alpha + bN*nc_merged1 + bM*logmass1;
    mu2 = alpha + bN*nc_merged2 + bM*logmass2;
    mu3 = alpha + bN*nc_merged3 + bM*logmass3;
    kcal1 ~ normal( mu1 , sigma );
    kcal2 ~ normal( mu2 , sigma );
    kcal3 ~ normal( mu3 , sigma );
}'''
*)

let ex_rethink = S_of_list [   Data("kcal1"); Data("kcal2"); Data("kcal3");
                               Data("neocortex1"); Data("neocortex2"); Data("neocortex3");
                               Data("logmass1"); Data("logmass2"); Data("logmass3");
                               Sample("alpha", Dist("normal",[Const(0.0);Const(10.0)]));
                               Sample("bN", Dist("normal",[Const(0.0);Const(10.0)]));
                               Sample("bM", Dist("normal",[Const(0.0);Const(10.0)]));
                               Sample("mu_nc", Dist("normal",[Const(0.5);Const(1.0)]));
                               Sample("sigma", Dist("cauchy",[Const(0.0);Const(1.0)]));
                               Sample("sigma_nc", Dist("cauchy",[Const(0.0);Const(1.0)]));
                               Let("nc_merged1", Var("neocortex1"));
                               Let("nc_merged2", Var("neocortex2"));
                               Let("nc_merged3", Var("neocortex3"));
                               Sample("nc_merged1", Dist("normal",[Var("mu_nc");Var("sigma_nc")]));
                               Sample("nc_merged2", Dist("normal",[Var("mu_nc");Var("sigma_nc")]));
                               Sample("nc_merged3", Dist("normal",[Var("mu_nc");Var("sigma_nc")]));
                               Let("mu1", Plus(Var("alpha"), Plus(Mul(Var("bN"),Var("nc_merged1")), Mul(Var("bM"),Var("logmass1")))));
                               Let("mu2", Plus(Var("alpha"), Plus(Mul(Var("bN"),Var("nc_merged2")), Mul(Var("bM"),Var("logmass2")))));
                               Let("mu3", Plus(Var("alpha"), Plus(Mul(Var("bN"),Var("nc_merged3")), Mul(Var("bM"),Var("logmass3")))));
                               Sample("kcal1", Dist("normal",[Var("mu1");Var("sigma")]));
                               Sample("kcal2", Dist("normal",[Var("mu2");Var("sigma")]));
                               Sample("kcal3", Dist("normal",[Var("mu3");Var("sigma")]));
                               ]


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

let example2 = P(DBlock(Declr("real", "y1")), 
                 TDBlock(VSeq(Declr("real", "alpha"),
                              Declr("real", "beta")), 
                         SSeq(Let("alpha", Const(0.1)), 
                              Let("beta", Const(0.1)))), 
                 PBlock(VSeq(Declr("real", "tau_y"),Declr("real", "mu_y"))), 
                 TPBlock(Declr("real", "sigma_y"),Let("sigma_y", Prim("pow",[Var("tau_y");Const(-0.5)]))), 
                 MBlock(VNone, SSeq(SSeq(Sample("tau_y", Dist("gamma",[Var("alpha"); Var("beta")])),
                                         Sample("mu_y", Dist("normal",[Const(0.0); Const(1.0)]))),
                                         Sample("y1", Dist("normal",[Var("mu_y"); Var("sigma_y")])))),
                 GQBlock(Declr("real", "variance_y"),Let("variance_y", Prim("pow", [Var("sigma_y");Const(2.0)]))))