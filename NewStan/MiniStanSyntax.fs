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
                | Sample of LValue * Dist
                | SSeq of Statements * Statements  
                | If of Exp * Statements * Statements
                | SNone

// S; S; E; S; 
// S; S; foo = E; S

// E ::= E+E | .... | x = E | ....
// 5 + x = 5; it's going to parse and fail type checking. 


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
    | Bool -> "bool"
    | Real -> "real"
    | Int -> "int"
    | Constrained(tp', size) -> sprintf "%s<%s>" (Type_pretty tp') (SizeToString size)
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
    | Sample (name, dist) -> "\t" + (LValue_pretty name) + " ~ " + D_pretty dist + ";\n"
    | SSeq (s1, s2) -> Statements_pretty s1 + Statements_pretty s2
    | If(e, s1, SNone) -> sprintf "\tif(%s){\n\t%s\t}" (E_pretty e) (Statements_pretty s1)
    | If(e, s1, s2) -> sprintf "\tif(%s){\n\t%s\n}\telse{\n\t%s}\n" (E_pretty e) (Statements_pretty s1) (Statements_pretty s2)
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


        
 