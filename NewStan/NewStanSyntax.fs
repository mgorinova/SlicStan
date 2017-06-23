module NewStanSyntax

type ArrSize = int 
type TypeLevel = LogProb | Data | Local | Model | GenQuant
type TypePrim = Real | Int | Array of TypePrim * ArrSize

type Type = TypePrim * TypeLevel

type Ide = string
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

type LValue = I of Ide | A of LValue * Exp

let ex_1dim: Exp = ArrElExp(Var("x"), Const(0.5))
let ex_2dim: Exp = ArrElExp(ArrElExp(Var("x"), Const(0.1)), Const(0.5))

let ex_1dim_lhs: LValue = A(I("x"), Const(0.1))
let ex_2dim_lhs: LValue = A(A(I("x"), Const(0.1)), Const(0.5))

type Dist = Dist of string * Exp list  // normal(E1, E2); gamma(E1, E2);
          | DCall of FunIde * Exp list

type S = Block of Arg * S //alpha convertible; make it have a single identifier
       | Sample of Ide * Dist // x ~ D
       | DataDecl of TypePrim * Ide * S //not up to alpha conversion  // data x; S
       | Assign of LValue * Exp // x = E 
       | Seq of S * S // S1; S2
       | Skip 
       | VCall of FunIde * Exp list


type FunDef = FunE of FunIde * Arg list * S * Type * Exp
            | FunD of FunIde * Arg list * S * Type * Dist
            | FunV of FunIde * Arg list * S

let name fundef =
    match fundef with 
    | FunE(n, _, _, _, _) -> n
    | FunD(n, _, _, _, _) -> n
    | FunV(n, _, _) -> n

type NewStanProg = FunDef list * S

let empty = Set.empty

let rec TPrim_pretty tp =
    match tp with
    | Real -> "real"
    | Int -> "int"
    | Array(t, n) -> (TPrim_pretty t) + (sprintf "[%d]" n)

let TLev_pretty tl =
    match tl with
    | LogProb -> failwith "unexpected"
    | Data -> "data"
    | Local -> "level"
    | Model -> "model"
    | GenQuant -> "quant"
    
let Type_pretty t =
    match t with 
    | tp, tl -> sprintf "%s %s" (TLev_pretty tl) (TPrim_pretty tp)

let rec E_pretty E =
  match E with
  | Var(x) -> x
  | Const(d) -> sprintf "%O" d
  | Arr(Es) -> sprintf "[ %s ]" (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))
  | ArrElExp(e1, e2) -> sprintf "%s[%s]" (E_pretty e1) (E_pretty e2) 
  | Plus(e1, e2) -> E_pretty e1 + " + " + E_pretty e2
  | Mul(e1, e2) -> E_pretty e1 + " * " + E_pretty e2
  | Prim(p,[]) -> sprintf "%s()" p
  | Prim(p,Es) -> sprintf "%s(%s)" p (List.reduce (fun s1 s2 -> s1+","+s2) (List.map E_pretty Es))
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
  | DataDecl(t, x, s) -> sprintf "%s%s %s;\n%s" ident (TPrim_pretty t) x (S_pretty ident s)
  | Block(env, S) -> sprintf "%s%A{\n%s\n%s}" ident env (S_pretty ("  " + ident) S) ident
  | Sample(x,D) -> sprintf "%s%s ~ %s;" ident x (D_pretty D)
  | Assign(lhs,E) -> sprintf "%s%s = %s;" ident (LValue_pretty lhs) (E_pretty E) //(LValue_pretty x)
  | Seq(S1,S2) -> sprintf "%s \n%s" (S_pretty ident S1) (S_pretty ident S2)
  | Skip -> ""
  | VCall(x,[]) -> sprintf "%s%s()" ident x 
  | VCall(x,Es) -> sprintf "%s%s(%s)" ident x (List.reduce (fun s1 s2 -> s1+", "+s2) (List.map E_pretty Es))


let rec List_pretty lst =
    match lst with
    | [] -> ""
    | [((p, _), n)] -> sprintf "%s %s" (TPrim_pretty p) n 
    | ((p, _), n)::ls -> sprintf "%s %s, %s" (TPrim_pretty p) n (List_pretty ls)

let rec DefList_pretty defs =
    match defs with
    | [] -> ""
    | p::ps -> match p with
               | FunE(name, args, s, _, e) -> sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (E_pretty e) (DefList_pretty ps)
               | FunD(name, args, s, _, d) -> sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (D_pretty d) (DefList_pretty ps)
               | FunV(name, args, s)    -> sprintf "def %s(%s){\n%s\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (DefList_pretty ps)

let rec NewStanProg_pretty prog = 
    match prog with 
    | defs, s -> sprintf "%s\n%s\n" (DefList_pretty defs) (S_pretty "" s)

// helper function to build a statement from a statement list
let rec SofList ss =
  match ss with
  | [] -> Skip
  | [s] -> s
  | s::ss' -> Seq(s, SofList ss')


let rec LValueBaseName (lhs: LValue): Ide =    
    match lhs with
    | I(name) -> name
    | A(lhs', _) -> LValueBaseName lhs'




