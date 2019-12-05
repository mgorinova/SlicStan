module SlicStanSyntax

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

type S = Decl of Arg * S //alpha convertible; make it have a single identifier // {(real, MODEL) x; S}
       | Sample of Exp * Dist // x ~ D // general case should be E ~ D
       | Assign of LValue * Exp // x = E 
       | If of Exp * S * S // if-else
       | For of Arg * ArrSize * ArrSize * S
       | Seq of S * S // S1; S2
       | Skip 
       | Elim of Ide list * Arg * S // Elim of message list, varaible to be eliminated and a statement
       | Message of Arg * Ide * S // match Arg with int<K> z: for (z in 1:K) S [target -> LValue[z]];
       | Generate of Ide list * Arg * S // Generate Arg using message list and statement


type FunDef = Fun of FunIde * Arg list * S * Arg


let distributions: (string * (TypePrim list * TypePrim)) list =
    ["normal", ([Real; Real], (Real)); // normal(mu, sigma)
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
     "categorical", ([Array(Real, AnySize)], (Int)); ]

let rngs: (string * (TypePrim list * TypePrim)) list =
    List.map (fun (name, (args, ret)) -> name + "_rng", (args, ret)) distributions

let lpdfs : (string * (TypePrim list * TypePrim)) list =
    List.map (fun (name, (args, ret)) -> 
                match ret with 
                | Real -> name + "_lpdf", (ret::args, Real)
                | Int -> name + "_lpmf", (ret::args, Real)
                | _ -> failwith "no support for multivariate distributions yet" 
              ) distributions

let prim_funcs =  [ "-", ([Real; Real], (Real));
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
                    "to_vector", ([Array(Real, AnySize)], Vector(AnySize));
                    "sum", ([Array(Real, AnySize)], Real);
                    ]

let Primitives: Map<string, TypePrim list * TypePrim> = 
    prim_funcs |> List.append distributions
               |> List.append rngs
               |> List.append lpdfs
               |> Map.ofList
                

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
    | Fun(n, _, _, _) -> n

type SlicStanProg = FunDef list * S

let empty = Map.empty

let rec LValueBaseName (lhs: LValue): Ide =    
    match lhs with
    | I(name) -> name
    | A(lhs', _) -> LValueBaseName lhs'

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
  | ArrElExp(e1, e2) -> ArrElExp_pretty E + "]" //sprintf "%s[%s]" (E_pretty e1) (E_pretty e2) 
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


and ArrElExp_pretty arr_el =
    match arr_el with 
    | ArrElExp(e1, e2) -> 
        match e1 with
        | ArrElExp _ -> sprintf "%s,%s" (ArrElExp_pretty e1) (E_pretty e2)
        | _ -> sprintf "%s%s" (ArrElExp_pretty e1) (E_pretty e2)
    | _ -> sprintf "%s[" (E_pretty arr_el)

and D_pretty D =
  match D with
  | Dist(p,[]) -> sprintf "%s()" p
  | Dist(p,Es) -> sprintf "%s(%s)" p (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))

let rec LValue_pretty (x:LValue) =
        match x with 
        | I(name) -> name
        | A(lhs, index) -> A_pretty x + "]" //sprintf "%s[%s]" (LValue_pretty lhs) (E_pretty index)
 
 and A_pretty (x:LValue) = 
    match x with 
    | A(lhs, i) -> 
        match lhs with
        | A _ -> sprintf "%s,%s" (A_pretty lhs) (E_pretty i)
        | _ -> sprintf "%s%s" (A_pretty lhs) (E_pretty i)
    | _ -> sprintf "%s[" (LValue_pretty x)

let rec assigns_syntax (S: S) : Set<Ide> =
    match S with
    | Decl((_, x), s) -> assigns_syntax s
    | Sample(x, _) -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns_syntax s1) (assigns_syntax s2)
    | For(x, l, u, s) -> assigns_syntax s
    | Seq(s1, s2) -> Set.union (assigns_syntax s1) (assigns_syntax s2)
    | Skip -> Set.empty

let Messages_pretty messages =
    if List.length messages = 0 then ""
    else if List.length messages = 1 then List.head messages
    else List.fold (fun s m -> sprintf "%s,%s" s m) "" messages

let rec S_pretty ident S =
  match S with
  | Decl(var, S) -> //
    let (p, l), n = var
    //if p <. Int && l = Model && (Set.contains (snd var) (assigns_syntax S) |> not)
    //then sprintf "%s%s %s %s{\n%s\n%s}" ident (TPrim_pretty p) (TLev_pretty l) n (S_pretty ("  " + ident) S) ident
    //else 
    sprintf "%s%s %s %s;\n%s\n%s" ident (TPrim_pretty p) (TLev_pretty l) n (S_pretty (ident) S) ident 
        
  | Sample(E, D) -> sprintf "%s%s ~ %s;" ident (E_pretty E) (D_pretty D)
  | Assign(lhs,E) -> 
    if LValueBaseName lhs = "target"
    then match E with 
         | Plus(Var("target"), e2) -> sprintf "%starget += %s;" ident (E_pretty e2) 
         | Prim("+", [ Var("target"); e2] ) -> sprintf "%starget += %s;" ident (E_pretty e2)
         | _ -> sprintf "%s%s = %s;" ident (LValue_pretty lhs) (E_pretty E) 
    else sprintf "%s%s = %s;" ident (LValue_pretty lhs) (E_pretty E) 
  | If(E, S1, Skip) -> sprintf "%sif(%s){\n%s\n%s}" ident (E_pretty E) (S_pretty ("  " + ident) S1) ident
  | If(E, S1, S2) -> sprintf "%sif(%s){\n%s\n%s}%selse{\n%s\n%s}" ident (E_pretty E) (S_pretty ("  " + ident) S1) ident ident (S_pretty ("  " + ident) S2) ident
  | For((t, x), lower, upper, S) -> sprintf "%sfor(%s %s in %s:%s){\n%s\n%s}" ident (Type_pretty t) (x) (SizeToString lower) (SizeToString upper) (S_pretty ("  " + ident) S) ident
  | Seq(S1,S2) -> sprintf "%s \n%s" (S_pretty ident S1) (S_pretty ident S2)
  | Skip -> ""
  | Elim(messages, var, S) ->
    let (p, _), n = var
    sprintf "%selim<%s>(%s %s){\n%s\n%s}" ident (Messages_pretty messages) (TPrim_pretty p) n (S_pretty ("  " + ident) S) ident
  | Message(arg, var, S) ->
    let (p, _), name = arg
    sprintf "%smessage<%s>(%s){\n%s\n%s}" ident name var (S_pretty ("  " + ident) S) ident
  | Generate(messages, var, S) ->
    let (p, _), n = var
    sprintf "%sgenerate<%s>(%s %s){\n%s\n%s}" ident (Messages_pretty messages) (TPrim_pretty p) n (S_pretty ("  " + ident) S) ident
    

let rec List_pretty lst =
    match lst with
    | [] -> ""
    | [((p, l), n)] -> sprintf "%s(%A) %s" (TPrim_pretty p) l n 
    | ((p, l), n)::ls -> sprintf "%s(%A) %s, %s" (TPrim_pretty p) l n (List_pretty ls)

let rec DefList_pretty defs =
    match defs with
    | [] -> ""
    | p::ps -> match p with Fun(name, args, s, ((tp, _), x)) -> 
                if tp = Unit then 
                    sprintf "def %s(%s){\n%s\n}\n%s" name (List_pretty args) (S_pretty "  " s) (DefList_pretty ps)
                else 
                    sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" name (List_pretty args) (S_pretty "  " s) (x) (DefList_pretty ps)

let rec SlicStanProg_pretty prog = 
    match prog with 
    | defs, s -> sprintf "%s\n%s\n" (DefList_pretty defs) (S_pretty "" s)
    

// helper function to build a statement from a statement list
let rec SofList ss =
  match ss with
  | [] -> Skip
  | [s] -> s
  | s::ss' -> Seq(s, SofList ss')


let rec BlockOfList (env, s) = 
    match env with 
    | [] -> s
    | x::xs -> Decl(x, BlockOfList (xs, s))


let BaseTypeLevel (tau: TypeLevel) =
    match tau with
    | Data -> true
    | Model -> true
    | GenQuant -> true
    | _ -> false


let typeLevelNames = Seq.initInfinite (fun index ->
        "l" + (string index))

let mutable cur = 0

let next() =
    let ret = Seq.item cur typeLevelNames
    cur <- cur + 1
    ret

let reset_levels() = cur <- 0 



