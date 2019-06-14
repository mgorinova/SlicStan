module NewStanSyntax

type ArrSize = N of int | SizeVar of string
let AnySize = N(-1)//

let SizeToString (n: ArrSize) =
    match n with
    | N(i) -> sprintf "%d" i
    | SizeVar(s) -> s

let NotAnySize (n: ArrSize) =
    match n with
    | N(-1) -> false
    | _ -> true

type TypeLevel = LevelVar of string | Data | Model | GenQuant | Lub of TypeLevel list | Glb of TypeLevel list
type TypePrim = Bool | Real | Int | Constrained of TypePrim * ArrSize 
              | Array of TypePrim * ArrSize | Vector of ArrSize | Matrix of ArrSize * ArrSize | Unit 

type Type = TypePrim * TypeLevel

type Ide = string // indentifier
type FunIde = string
type Arg = Type * Ide

type Exp = Var of Ide
         | Const of double
         | Arr of Exp list // [E1, ... En]
         | ArrElExp of Exp * Exp // E[E]
         | Plus of Exp * Exp // E1 + E2
         | Mul of Exp * Exp // E1 * E2
         | Prim of string * Exp list // sqrt(E1, E2)
         | ECall of FunIde * Exp list 

// M' = matrix transpose
// inverse(M) = inverse
// M1 * M2 = matrix multiplication
// M1 / M2 = matrix division is provided, which is much more arithmetically stable than inversion
// M1 .* M2 = elementwise multiplication
// M1 ./ M2 = elementwise devision

type LValue = I of Ide | A of LValue * Exp // x[E]

type Dist = Dist of string * Exp list  // normal(E1, E2); gamma(E1, E2);
          | DCall of FunIde * Exp list

type S = Block of Arg * S //alpha convertible; make it have a single identifier // {(real, MODEL) x; S}
       | Sample of LValue * Dist // x ~ D // general case should be E ~ D
       | DataDecl of TypePrim * Ide * S //not up to alpha conversion  // data x; S // should be just a special case of Block(((real, DATA), x), S)
       | Assign of LValue * Exp // x = E 
       | If of Exp * S * S // if-else
       | For of Arg * ArrSize * ArrSize * S
       | Seq of S * S // S1; S2
       | Skip 
       | VCall of FunIde * Exp list


// there should be instead only one type of function definition:
// type FunDef = Fun of FunIde * Arg list * S * Type
// (void, GENQUANT) is a Type
// Distrubution: E ~ foo(E') is a syntactic sugar target += foo(E | E')
// the definition of foo(real x | real theta)
type FunDef = FunE of FunIde * Arg list * S * Exp
            | FunD of FunIde * Arg list * S * Dist
            | FunV of FunIde * Arg list * S * unit


let Primitives: Map<string, TypePrim list * TypePrim> = 
    Map.ofList ["normal", ([Real; Real], (Real)); // normal(mu, sigma)
                "lognormal", ([Real; Real], (Real));
                "gamma", ([Real; Real], (Real)); // gamma(alpha, beta)
                "inv_gamma", ([Real; Real], (Real));
                "uniform", ([Real; Real], (Real));
                "exp_mod_normal", ([Real; Real; Real], (Real)); // exp_mod_normal(mu, sigma, lambda)
                "student_t", ([Real; Real; Real], (Real)); //student_t(nu, mu, sigma)
                "cauchy", ([Real; Real], (Real)); // cauchy(mu, sigma)
                "binomial_logit", ([Int; Real], (Int)); 
                "pareto", ([Real; Real], (Real)); 
                "beta", ([Real; Real], (Real)); 
                "poisson", ([Real], (Int)); 
                "poisson_log", ([Real], (Int)); 
                "categorical", ([Array(Real, AnySize)], (Int));
                "-", ([Real; Real], (Real));
                "+", ([Real; Real], (Real));
                "*", ([Real; Real], (Real));
                "/", ([Real; Real], (Real));
                ">", ([Real; Real], (Bool));
                "<", ([Real; Real], (Bool));
                ">=", ([Real; Real], (Bool));
                "<=", ([Real; Real], (Bool));
                ".*", ([Real; Real], (Real));
                "./", ([Real; Real], (Real));
                "pow", ([Real; Real], (Real));
                "sqrt", ([Real], Real);
                "exp", ([Real], Real);
                "log", ([Real], Real);
                "inv", ([Real], Real);
                "mean", ([Real], Real);
                "sd", ([Real], Real);
                "cholesky_decompose", ([Matrix(AnySize, AnySize)], Matrix(AnySize, AnySize));
                "multi_normal", ([Vector(AnySize); Matrix(AnySize, AnySize)], (Vector(AnySize)));
                "num_elements", ([Vector(AnySize)], Int);
                ]


let (<=) (l1:TypeLevel) (l2:TypeLevel) =
    match l1, l2 with
    | Data, _ -> true
    | Model, Data -> false
    | Model, _ -> true
    | _, GenQuant -> true 
    | GenQuant, Data -> false    
    | GenQuant, Model -> false  
    | _ -> true

let rec (==) (p1: TypePrim) (p2: TypePrim) : bool =
    match p1, p2 with
    | Int, Int -> true
    | Real, Real -> true
    | Bool, Bool -> true
    | Constrained(tp1, n1), Constrained(tp2, n2) -> n1 = n2 && tp1 == tp2 
    //| Constrained(tp1, n1), tp2 -> tp1 == tp2 // FIXME: this should probably be done in a smarter way
    //| tp1, Constrained(tp2, n2) -> tp1 == tp2 // FIXME: this should probably be done in a smarter way
    | Vector(n1), Vector(n2) -> n1 = AnySize || n2 = AnySize || n1 = n2
    | Matrix(m1, n1), Matrix(m2, n2) -> (n1 = AnySize || n2 = AnySize || n1 = n2) && (m1 = AnySize || m2 = AnySize || m1 = m2)
    | Array(tp1, n1), Array(tp2, n2) -> (n1 = AnySize || n2 = AnySize || n1 = n2) && (tp1 == tp2)
    | Array(tp, _), p -> p = tp // vectorisation
    | Vector _, p -> p = Real   // vectorisation
    | _ -> false

let rec (<.) (p1: TypePrim) (p2: TypePrim) : bool =
    p2 == p1 || (
    match p1, p2 with
    | Int, Real -> true 
    | Constrained(t1, n1), Constrained(t2, n2) -> 
        // The second part is commented, so that int<2> = foo(int<4>) is OK. 
        // Otherwise weird, because what if we want int<2> = int<4> / 2 ?
        t1 <. t2 // && (function | N(n1'), N(n2') -> n1' < n2' || n1' = n2' | _ -> true) (n1, n2)
    | Constrained(t1, _), t2 -> t1 <. t2
    | t1, Constrained(t2, _) -> 
        // Perhaps a bit weird, but otherwise things such as int<3> = foo(int)
        // are disallowed and that may be weird.
        t1 <. t2 
    | _ -> false
    )

let name fundef =
    match fundef with 
    | FunE(n, _, _, _) -> n
    | FunD(n, _, _, _) -> n
    | FunV(n, _, _, _) -> n

type NewStanProg = FunDef list * S

let empty = Set.empty

let rec TPrim_pretty tp =
    match tp with
    | Real -> "real"
    | Int -> "int"
    | Bool -> "int"
    | Constrained(tp', size) -> sprintf "%s<%s>" (TPrim_pretty tp') (SizeToString size)
    | Array(t, n) -> (TPrim_pretty t) + (if NotAnySize(n) then (sprintf "[%s]" (SizeToString n)) else "[]")
    | Vector(n) -> if n > AnySize then (sprintf "vector[%s]" (SizeToString n)) else "vector"
    | Matrix(n1, n2) -> if NotAnySize(n1) && NotAnySize(n2) then (sprintf "matrix[%s, %s]" (SizeToString n1) (SizeToString n2)) else "matrix"
    | Unit -> "unit"

let rec TLev_pretty tl =
    match tl with
    | Data -> "data"
    | Model -> "model"
    | GenQuant -> "quant"
    | LevelVar(s) -> s
    | Lub(ls) -> sprintf "lub %A" (List.map TLev_pretty ls)
    | Glb(ls) -> sprintf "glb %A" (List.map TLev_pretty ls)
 

let TLev_compact_pretty tl =
    match tl with
    | Data -> "data"
    | Model -> "model"
    | GenQuant -> "quant"
    | LevelVar(s) -> ""
    | Lub(ls) -> ""
    | Glb(ls) -> ""

let Type_pretty t =
    match t with 
    | tp, tl -> sprintf "%s %s" (TLev_pretty tl) (TPrim_pretty tp)

let rec E_pretty E =
  match E with
  | Var(x) -> x
  | Const(d) -> sprintf "%O" d
  | Arr(Es) -> sprintf "[ %s ]" (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))
  | ArrElExp(e1, e2) -> sprintf "%s[%s]" (E_pretty e1) (E_pretty e2) 
  | Plus(e1, e2) -> sprintf "(%s + %s)" (E_pretty e1) (E_pretty e2)
  | Mul(e1, e2) -> sprintf "%s * %s" (E_pretty e1) (E_pretty e2)
  | Prim(p,[]) -> sprintf "%s()" p
  | Prim(p,Es) -> 
    if (p = "+" || p = "*" || p = "-" || p = "/" || 
        p = ".*" || p = "./" ||
        p = ">" || p = "<" || p = ">=" || p = "<=") && Es.Length = 2 then sprintf "(%s %s %s)" (E_pretty Es.[0]) p (E_pretty Es.[1])
    else sprintf "%s(%s)" p (List.reduce (fun s1 s2 -> s1+","+s2) (List.map E_pretty Es))
  | ECall(x,[]) -> sprintf "%s()" x 
  | ECall(x,Es) -> sprintf "%s(%s)" x (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))


and D_pretty D =
  match D with
  | Dist(p,[]) -> sprintf "%s()" p
  | Dist(p,Es) -> sprintf "%s(%s)" p (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))
  | DCall(x,[]) -> sprintf "%s()" x 
  | DCall(x,Es) -> sprintf "%s(%s)" x (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))

let rec LValue_pretty (x:LValue) =
        match x with 
        | I(name) -> name
        | A(lhs, index) -> sprintf "%s[%s]" (LValue_pretty lhs) (E_pretty index)

let rec S_pretty ident S =
  match S with
  | DataDecl(t, x, s) -> sprintf "%sdata %s %s;\n%s" ident (TPrim_pretty t) x (S_pretty ident s)
  | Block(env, S) -> //
    let (p, l), n = env
    sprintf "%s%s %s %s;\n%s" ident (TLev_compact_pretty l) (TPrim_pretty p) n (S_pretty ident  S)
  | Sample(x, D) -> sprintf "%s%s ~ %s;" ident (LValue_pretty x) (D_pretty D)
  | Assign(lhs,E) -> 
    match E with 
    | Plus(e1, e2) ->
        let left = LValue_pretty lhs 
        let right = E_pretty e1
        if left = right then 
            sprintf "%s%s += %s" ident left (E_pretty e2)
        else sprintf "%s%s = %s + %s" ident left right (E_pretty e2)
    | _ -> sprintf "%s%s = %s;" ident (LValue_pretty lhs) (E_pretty E) //(LValue_pretty x)
  | If(E, S1, Skip) -> sprintf "%sif(%s){\n%s\n%s}" ident (E_pretty E) (S_pretty ("  " + ident) S1) ident
  | If(E, S1, S2) -> sprintf "%sif(%s){\n%s\n%s}%selse{\n%s\n%s}" ident (E_pretty E) (S_pretty ("  " + ident) S1) ident ident (S_pretty ("  " + ident) S2) ident
  | For((t, x), lower, upper, S) -> sprintf "%sfor(%s %s in %s:%s){\n%s\n%s}" ident (Type_pretty t) (x) (SizeToString lower) (SizeToString upper) (S_pretty ("  " + ident) S) ident
  | Seq(S1,S2) -> sprintf "%s \n%s" (S_pretty ident S1) (S_pretty ident S2)
  | Skip -> ""
  | VCall(x,[]) -> sprintf "%s%s()" ident x 
  | VCall(x,Es) -> sprintf "%s%s(%s)" ident x (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))


let rec List_pretty lst =
    match lst with
    | [] -> ""
    | [((p, l), n)] -> sprintf "%s(%A) %s" (TPrim_pretty p) l n 
    | ((p, l), n)::ls -> sprintf "%s(%A) %s, %s" (TPrim_pretty p) l n (List_pretty ls)

let rec DefList_pretty defs =
    match defs with
    | [] -> ""
    | p::ps -> match p with
               | FunE(name, args, s, e) -> sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (E_pretty e) (DefList_pretty ps)
               | FunD(name, args, s, d) -> sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (D_pretty d) (DefList_pretty ps)
               | FunV(name, args, s, _)    -> sprintf "def %s(%s){\n%s\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (DefList_pretty ps)

let rec NewStanProg_pretty prog = 
    match prog with 
    | defs, s -> sprintf "%s\n%s\n" (DefList_pretty defs) (S_pretty "" s)



let typeLevelNames = Seq.initInfinite (fun index ->
        "l" + (string index))

let mutable cur = 0

let next() =
    let ret = Seq.nth cur typeLevelNames
    cur <- cur + 1
    ret

let reset_levels() = cur <- 0 



