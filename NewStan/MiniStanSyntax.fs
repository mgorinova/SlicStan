module MiniStanSyntax

open NewStanSyntax

type Ide = NewStanSyntax.Ide 
//type T = NewStan.T
//type ArrEl = NewStan.ArrEl
type Exp = NewStanSyntax.Exp 
type Dist = NewStanSyntax.Dist 

type LValue = NewStanSyntax.LValue

type TIde = TypePrim

type Funcs = string // TODO: define funcs

type VarDecls = Declr of TIde * Ide // TODO: add type constrains
              | VSeq of VarDecls * VarDecls  
              | VNone

type Statements = Let of LValue * Exp // TODO: add more statements
                | Sample of Ide * Dist
                | SSeq of Statements * Statements  
                | SNone


type Data = DBlock of VarDecls | DNone
type TData = TDBlock of VarDecls * Statements | TDNone
type Params = PBlock of VarDecls | PNone
type TParams = TPBlock of VarDecls * Statements | TPNone
type Model = MBlock of VarDecls * Statements
type GenQuant = GQBlock of VarDecls * Statements | GQNone

type Block = VarDecls * Statements 

type Prog = P of Data * TData * Params * TParams * Model * GenQuant


let E_pretty = NewStanSyntax.E_pretty
let D_pretty = NewStanSyntax.D_pretty

let rec Type_pretty typeprim =
    match typeprim with 
    | Real -> "real"
    | Int -> "int"
    | Array(t, n) -> (Type_pretty t) + (if NotAnySize(n) then (sprintf "[%s]" (SizeToString n)) else "[]")
    | Vector(n) -> (if NotAnySize(n) then (sprintf "vector[%s]" (SizeToString n)) else "vector")
    | Matrix(n, m) -> (if NotAnySize(n) then 
                            if NotAnySize(m) then (sprintf "matrix[%s,%s]" (SizeToString n) (SizeToString m)) else (sprintf "matrix[%s,]" (SizeToString n)) 
                       else if NotAnySize(m) then (sprintf "matrix[,%s]" (SizeToString m)) else "matrix")
    | Unit -> "()"

let rec Decls_pretty decls =
    match decls with 
    | Declr (typestr, name) -> "\t" + (Type_pretty typestr) + " " + name + ";\n"
    | VSeq (d1, d2) ->  Decls_pretty d1 + Decls_pretty d2
    | VNone -> ""

let rec Statements_pretty decls =
    match decls with 
    | Let (lhs, expr) -> "\t" + LValue_pretty lhs + " = " + E_pretty expr + ";\n"
    | Sample (name, dist) -> "\t" + name + " ~ " + D_pretty dist + ";\n"
    | SSeq (s1, s2) ->  Statements_pretty s1 + Statements_pretty s2
    | SNone -> ""

let Data_pretty d =
    match d with 
    | DBlock (ds) -> Decls_pretty ds
    | DNone -> ""

let TData_pretty d =
    match d with 
    | TDBlock (ds, ss) -> Decls_pretty ds + Statements_pretty ss
    | TDNone -> ""

let Params_pretty d =
    match d with 
    | PBlock (ds) -> Decls_pretty ds
    | PNone -> ""

let TParams_pretty d =
    match d with 
    | TPBlock (ds, ss) -> Decls_pretty ds + Statements_pretty ss
    | TPNone -> ""

let Model_pretty d =
    match d with 
    | MBlock (ds, ss) -> Decls_pretty ds + Statements_pretty ss

let GenQuant_pretty d =
    match d with 
    | GQBlock (ds, ss) -> Decls_pretty ds + Statements_pretty ss
    | GQNone -> ""

let rec Prog_pretty (p:Prog) : string= 
    match p with
    | P(d, td, p, tp, m, gq) -> 
        let funcs  = "" // TODO add function pretty printing
        let data   = if Data_pretty d <> ""      then ("data {\n"+ Data_pretty d + "\n}\n") else ""
        let tdata  = if TData_pretty td <> ""    then ("transformed data {\n"+ TData_pretty td + "\n}\n") else ""
        let parms  = if Params_pretty p <> ""    then ("parameters {\n"+ Params_pretty p + "\n}\n") else ""
        let tparms = if TParams_pretty tp <> ""  then ("transformed parameters {\n"+ TParams_pretty tp + "\n}\n") else ""
        let model  =                                   "model {\n" + Model_pretty m + "\n}\n"
        let gens   = if GenQuant_pretty gq <> "" then ("generated quantities {\n"+ GenQuant_pretty gq + "\n}") else ""

        funcs + data + tdata + parms + tparms + model + gens



let example = P (DNone, 
                 TDNone, 
                 PNone, 
                 TPNone, 
                 MBlock(VNone, Sample("x",Dist("normal",[Const(0.0); Const(1.0)]))),
                 GQNone)
                 
(*  data y1;
    alpha = 0.1; 
    beta = 0.1; 
    tau_y ~ gamma(alpha,beta); 
    sigma_y = pow(tau_y,-0.5); 
    mu_y ~ normal(0,1); 
    y1 ~ normal(mu_y,sigma_y); 
    variance_y = *(sigma_y,sigma_y);
*)

(*let example2 = P(DBlock(Declr(Real, "y1")), 
                 TDBlock(VSeq(Declr(Real, "alpha"),
                              Declr(Real, "beta")), 
                         SSeq(Let("alpha", Const(0.1)), 
                              Let("beta", Const(0.1)))), 
                 PBlock(VSeq(Declr(Real, "tau_y"),Declr(Real, "mu_y"))), 
                 TPBlock(Declr(Real, "sigma_y"),Let("sigma_y", Prim("pow",[Var("tau_y");Const(-0.5)]))), 
                 MBlock(VNone, SSeq(SSeq(Sample("tau_y", Dist("gamma",[Var("alpha"); Var("beta")])),
                                         Sample("mu_y", Dist("normal",[Const(0.0); Const(1.0)]))),
                                         Sample("y1", Dist("normal",[Var("mu_y"); Var("sigma_y")])))),
                 GQBlock(Declr(Real, "variance_y"),Let("variance_y", Prim("pow", [Var("sigma_y");Const(2.0)]))))*)