module NewStanSyntax

type TypeLevel = LogProb | Data | Local | Model | GenQuant
type TypePrim = Real

type Type = TypePrim * TypeLevel

type Ide = string
type FunIde = string
type Arg = Type * Ide
type Env = (Type * Ide) Set


type Exp = Var of Ide
         | Const of double
         | Plus of Exp * Exp
         | Mul of Exp * Exp
         | Prim of string * List<Exp>
         | ECall of FunIde * Exp list

type Dist = Dist of string * List<Exp>
          | DCall of FunIde * Exp list

type S = Block of Env * S //alpha convertible; make it have a single identifier
       | Sample of Ide * Dist
       | DataDecl of TypePrim * Ide * S //not up to alpha conversion 
       | Assign of Ide * Exp
       | Seq of S * S
       | Skip
       | VCall of FunIde * Exp list


type FunDef = FunE of FunIde * Arg list * S * Exp
            | FunD of FunIde * Arg list * S * Dist
            | FunV of FunIde * Arg list * S

let name fundef =
    match fundef with 
    | FunE(n, _, _, _) -> n
    | FunD(n, _, _, _) -> n
    | FunV(n, _, _) -> n

type NewStanProg = FunDef list * S

let empty = Set.empty

let TPrim_pretty tp =
    match tp with
    | Real -> "real"

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

let rec S_pretty ident S =
  match S with
  | DataDecl(t, x, s) -> sprintf "%s%s %s\n%s;" ident (TPrim_pretty t) x (S_pretty ident s)
  | Block(env, S) -> sprintf "%s%A{\n%s\n%s}" ident env (S_pretty ("  " + ident) S) ident
  | Sample(x,D) -> sprintf "%s%s ~ %s;" ident x (D_pretty D)
  | Assign(x,E) -> sprintf "%s%s = %s;" ident x (E_pretty E)
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
               | FunE(name, args, s, e) -> sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" 
                                                    name (List_pretty args) (S_pretty "  " s) (E_pretty e) (DefList_pretty ps)
               | FunD(name, args, s, d) -> sprintf "def %s(%s){\n%s\n  return %s;\n}\n%s" 
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


let rec get_fun x defs : FunDef = 
    match defs with
    | [] -> failwith (sprintf "function %s not defined" x) 
    | d::ds ->
        if (name d = x) then d
        else get_fun x ds


/// Given a list of function argument types and names
/// and a list of expressions corresponding to those,
/// returns a statement S, which is a sequence of 
/// assignments (variable <- expression).
let assign_all (lhs: Arg list) (rhs:Exp list) : S =
    let zipped = List.zip lhs rhs

    let rec _assign tuples =
        match tuples with
        | [] -> Skip
        | ((t,v), r)::zs -> Seq(Assign(v,r), _assign zs)

    _assign zipped

/// Transforms an expression to a block and a return value.
/// TODO: make sure to deal with bound variables.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
let rec elaborate_E (defs: FunDef list) (exp: Exp) : S*Exp =
    match exp with 
    | ECall(x, Es) -> 
        let f = get_fun x defs
        match f with 
        | FunE(_, args, s, ret) -> 
            let b = elaborate_S defs empty s
            match b with
            | Block(env', s') ->
                Block(set args, Seq((assign_all args Es), s')), ret
            | _ -> failwith "unexpected in elaborate E function"

        | FunD(_) -> failwith "function is expected to return an expression, but returns a distribution instead"
        | FunV(_) -> failwith "function is expected to return a distribution, but returns void instead"

    | e -> Block(empty, Skip), e

/// Transforms a distribution to a block and a return value.
/// TODO: make sure to deal with bound variables.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
and elaborate_D (defs: FunDef list) (dist: Dist) : S*Dist =
    match dist with
    | DCall(x, Es) ->
        let f = get_fun x defs
        match f with 
        | FunD(_, args, s, ret) -> 
            let b = elaborate_S defs empty s
            match b with
            | Block(env', s') ->
                Block(set args, Seq((assign_all args Es), s')), ret
            | _ -> failwith "unexpected in elaborate E function"

        | FunE(_) -> failwith "function is expected to return a distribution, but returns an expression instead"
        | FunV(_) -> failwith "function is expected to return a distribution, but returns void instead"
    
    | d -> Block(empty, Skip), d

/// Transforms a statement to a block and a return value.
/// TODO: make sure to deal with bound variables.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
and elaborate_S (defs: FunDef list ) (env: Env) (s: S) : S =
    match s with 
    | DataDecl(t,str, s) -> 
        let newenv = Set.add ((t, Data),str) env
        Block(set [((t, Data),str)], (DataDecl(t,str,elaborate_S defs newenv s)))
    | Seq(s1, s2) -> 
        let b1 = elaborate_S defs env s1
        let b2 = elaborate_S defs env s2
        match b1,b2 with 
        | Block(env1, s1'), Block(env2, s2') ->             
            Block(Set.union env1 env2, Seq(s1', s2'))
        | _ -> failwith "unexpected in elaborate S seq"
    | Assign(x, e) -> 
        let b, e' = elaborate_E defs e 
        match b with 
        | Block(env', s') -> Block(Set.union env env', Seq(s', Assign(x, e')))
        | b' -> failwith (sprintf "unexpected in elabotare S assign: %A" b')
    | Sample(x, d) -> 
        let b, d' = elaborate_D defs d 
        match b with 
        | Block(env', s') -> Block(Set.union env env', Seq(s', Sample(x, d')))
        | b' -> failwith (sprintf "unexpected in elabotare S assign: %A" b')
    | VCall(x, es) -> 
        let f = get_fun x defs
        match f with 
        | FunV(_, args, s) -> 
            let b = elaborate_S defs empty s
            match b with
            | Block(env', s') -> 
                Block(set args, Seq((assign_all args es), s'))
            | _ -> failwith "unexpected in elaborate S function void"
        | FunE(_) -> failwith "function was not expected to have a return value, but returns an expression instead"
        | FunD(_) -> failwith "function was not expected to have a return value, but returns a distribution instead"

    | Block(env', s') ->
        let enew = Set.union env' env
        Block(empty, elaborate_S defs enew s')
    | s' -> 
        Block(env, s')



let elaborate_NewStanProg (prog: NewStanProg) : S =
    match prog with
    | defs, s -> 
        elaborate_S defs empty s
            

let rec check_data_and_model (S: S) : string list * string list = 
  match S with
  | DataDecl(t,x,s) ->
    let next_d, next_m = check_data_and_model s
    (x::next_d, next_m)
  | Sample(x,D) -> ([], [x])
  | Assign(x,E) -> ([], [])  
  | Seq(S1,S2) -> 
        let s1d, s1p = check_data_and_model S1
        let s2d, s2p = check_data_and_model S2
        (List.append s1d s2d, List.append s1p s2p)

  | Skip -> ([], [])
  | Block(env, s) -> 
    let next_d, next_m = check_data_and_model s
    (next_d, next_m)


