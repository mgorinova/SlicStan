module MiniStanSyntax

open SlicStanSyntax

type Ide = SlicStanSyntax.Ide 
//type T = SlicStan.T
//type ArrEl = SlicStan.ArrEl
type Exp = SlicStanSyntax.Exp 
type Dist = SlicStanSyntax.Dist 

type LValue = SlicStanSyntax.LValue

type TIde = TypePrim

type Funcs = string // TODO: define funcs

type VarDecls = Declr of TIde * Ide // TODO: add type constrains
              | VSeq of VarDecls * VarDecls  
              | VNone

type Statements = Let of LValue * Exp // TODO: add more statements
                | Sample of LValue * Dist
                | SSeq of Statements * Statements  
                | If of Exp * Statements * Statements
                | For of Ide * ArrSize * ArrSize * Statements
                | LocalDecl of TIde * Ide * Statements
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

type MiniStanProg = P of Data * TData * Params * TParams * Model * GenQuant


let E_pretty = SlicStanSyntax.E_pretty
let D_pretty = SlicStanSyntax.D_pretty

let rec Array_pretty arr_type =
    match arr_type with 
    | Array(t, n) -> 
        match t with
        | Array _ -> sprintf "%s,%s" (Array_pretty t) (SizeToString n)
        | _ -> sprintf "%s%s" (Array_pretty t) (SizeToString n)
    | _ -> sprintf "%s[" (Type_pretty arr_type)

and Type_pretty typeprim =
    match typeprim with 
    | Bool -> "bool"
    | Real -> "real"
    | Int -> "int"
    | Constrained(tp', size) -> sprintf "%s<lower=1,upper=%s>" (Type_pretty tp') (SizeToString size)
    | Array(t, n) -> (Array_pretty typeprim) + "]"// (if NotAnySize(n) then (sprintf "[%s]" (SizeToString n)) else "[]")
    | Vector(n) -> (if NotAnySize(n) then (sprintf "vector[%s]" (SizeToString n)) else "vector")
    | Matrix(n, m) -> (if NotAnySize(n) then 
                            if NotAnySize(m) then (sprintf "matrix[%s,%s]" (SizeToString n) (SizeToString m)) else (sprintf "matrix[%s,]" (SizeToString n)) 
                       else if NotAnySize(m) then (sprintf "matrix[,%s]" (SizeToString m)) else "matrix")
    | Unit -> "()"

let rec Decls_pretty decls =
    match decls with 
    | Declr (typestr, name) -> 
        match typestr with 
        //| Array(t, n) -> "\t" + (Type_pretty t) + " " + name + "[" + (SizeToString n) + "];\n"
        | _ -> "\t" + (Type_pretty typestr) + " " + name + ";\n"
    | VSeq (d1, d2) ->  Decls_pretty d1 + Decls_pretty d2
    | VNone -> ""

let rec Statements_pretty ident decls =
    match decls with 
    | Let (lhs, expr) -> ident +  LValue_pretty lhs + " = " + E_pretty expr + ";\n"
    | Sample (lhs, dist) -> "\t" + (LValue_pretty lhs) + " ~ " + D_pretty dist + ";\n"
    | SSeq (s1, s2) -> Statements_pretty ident s1 + Statements_pretty ident s2
    | If(e, s1, SNone) -> sprintf "%sif(%s){\n%s\n%s}" ident (E_pretty e) (Statements_pretty ident s1) ident
    | If(e, s1, s2) -> sprintf "%sif(%s){\n%s\n%s}\n%selse{\n%s\n%s}" ident (E_pretty e) (Statements_pretty (ident+"\t") s1) ident ident (Statements_pretty (ident+"\t") s2) ident
    | For(x, l, u, s) -> sprintf "%sfor(%s in %s : %s){\n%s\n%s}" ident (x) (SizeToString l) (SizeToString u) (Statements_pretty (ident+"\t") s) ident
    | LocalDecl(t, x, s) -> sprintf "%s%s %s;\n%s" ident (Type_pretty t) x (Statements_pretty ident s)
    | SNone -> ""

let Data_pretty d =
    match d with 
    | DBlock (ds) -> Decls_pretty ds
    | DNone -> ""

let TData_pretty d =
    match d with 
    | TDBlock (ds, ss) -> Decls_pretty ds + Statements_pretty "\t" ss
    | TDNone -> ""

let Params_pretty d =
    match d with 
    | PBlock (ds) -> Decls_pretty ds
    | PNone -> ""

let TParams_pretty d =
    match d with 
    | TPBlock (ds, ss) -> Decls_pretty ds + Statements_pretty "\t" ss
    | TPNone -> ""

let Model_pretty d =
    match d with 
    | MBlock (ds, ss) -> Decls_pretty ds + Statements_pretty "\t" ss

let GenQuant_pretty d =
    match d with 
    | GQBlock (ds, ss) -> Decls_pretty ds + Statements_pretty "\t" ss
    | GQNone -> ""

let rec Prog_pretty (p:MiniStanProg) : string= 
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
