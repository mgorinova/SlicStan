module Elaborate

open NewStanSyntax
open System.Runtime.CompilerServices

type Env = (Type * Ide) Set 
type Dict = Map<Ide,Ide> 

type Ret = ERet of Exp | DRet of Dist | None 

[<Extension>]
type Map<'Key,'Value when 'Key : comparison> with 
    static member safeFind k dict =
        if Map.containsKey k dict then (Map.find k dict)
        else k
    static member values dict =
        let lst = Map.toList dict
        List.map (fun (k,v) -> v) lst      


/// Returns the function x from  
/// the definition list defs.
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


/// Creates a nested blick statement S from 
/// a list of the veriables in scope (env), 
/// and an inner statement (body).
/// Example: block_from_list_env ({x,y}, S1) 
/// Returns: Block(x, Block(y, S1)).
let rec block_from_list_env (env: List<Arg>, body) =    
    List.fold (fun s v -> Block(v, s)) body env 


let create_dict (env: Env) (args: List<Arg>) (locals: List<Arg>):Dict =
    let prime (v:Ide) = 
        v + "p"

    let rec fresh (env: Env) (v:Ide) =
        if Set.fold (fun s (t,name) -> s || (v=name)) false env 
        then fresh env (prime v)
        else v

    let lst = (List.map (fun (t, a) -> (a, fresh env a)) (args @ locals))
    Map.ofList lst
 
let get_locals (s: S) : List<Arg> =
    let rec rec_locals s acc = 
        match s with
        | Block(env, s') ->  rec_locals s' (env::acc)
        | Seq(s1, s2) -> rec_locals s1 (rec_locals s2 acc)
        | _ -> acc

    rec_locals s []

/// Renames the single variable arg, as specified by dict.
let rec rename_arg (dict:Dict) (arg: Arg):Arg =
    match arg with t, x -> t, Map.safeFind x dict

/// Renames all bound variables in e, as specified by dict.
let rec rename_E (dict:Dict) (e: Exp):Exp =
    match e with
    | Var(v) -> 
        Var(Map.safeFind v dict)
    | Const(n) -> Const(n) 
    | Plus(e1, e2) -> Plus(rename_E dict e1, rename_E dict e2)
    | Mul(e1, e2) -> Mul(rename_E dict e1, rename_E dict e2)
    | Prim(name, Es) -> Prim(name, List.map (rename_E dict) Es)
    | ECall(name, Es) -> ECall(name, List.map (rename_E dict) Es)

/// Renames all bound variables in d, as specified by dict.
let rec rename_D (dict:Dict) (d: Dist):Dist =
    match d with
    | Dist(name, Es) -> Dist(name, List.map (rename_E dict) Es)
    | DCall(name, Es) -> DCall(name, List.map (rename_E dict) Es) // this shouldn't be possible


/// Renames all bound variables in s, as specified by dict.
let rec rename_S (dict:Dict) (s: S):S =
    match s with
    | DataDecl(t, x, s') -> DataDecl(t, Map.safeFind x dict, rename_S dict s')
    | Block(env, s') -> Block(rename_arg dict env, rename_S dict s')
    | Sample(x, d) -> Sample(Map.safeFind x dict, rename_D dict d)
    | Assign(x, e) -> Assign(Map.safeFind x dict, rename_E dict e)
    | Seq(s1, s2) -> Seq(rename_S dict s1, rename_S dict s2)
    | VCall(x, []) -> VCall(x,[])
    | VCall(x, Es) -> VCall(x, List.map (rename_E dict) Es)
    | Skip -> Skip
  
    
/// Transforms an expression to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
let rec elaborate_E (defs: FunDef list) (env: Env) (exp: Exp) : S*Exp =
    match exp with 
    | ECall(x, Es) -> 
        let f = get_fun x defs
        match f with 
        | FunE(_, args, s, ret) ->             
            let args', s', ret' = elaborate_F defs env f
            let body = Seq((assign_all args' Es), s')
            
            match ret' with
            | ERet(r') ->
                block_from_list_env (args', body), r'
            | _ -> failwith "unexpected"

        | FunD(_) -> failwith "function is expected to return an expression, but returns a distribution instead"
        | FunV(_) -> failwith "function is expected to return a distribution, but returns void instead"

    | Plus(E1, E2) -> 
        let s1, e1 = elaborate_E defs env E1
        let s2, e2 = elaborate_E defs env E2
        Seq(s1, s2), Plus(e1, e2)
    | Mul(E1, E2) ->
        let s1, e1 = elaborate_E defs env E1
        let s2, e2 = elaborate_E defs env E2
        Seq(s1, s2), Mul(e1, e2)
    | Prim(name, Es) ->
        let ss, es = List.map (elaborate_E defs env) Es |> List.unzip
        SofList ss, Prim(name, es)

    | e -> Skip, e 

/// Transforms a distribution to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
and elaborate_D (defs: FunDef list) (env: Env) (dist: Dist) : S*Dist =
    match dist with
    | DCall(x, Es) ->
        let f = get_fun x defs
        match f with 
        | FunD(_, args, s, ret) -> 
            let args', s', ret' = elaborate_F defs env f
            let body = Seq((assign_all args' Es), s')
            
            match ret' with
            | DRet(r') ->
                block_from_list_env (args', body), r'
            | _ -> failwith "unexpected"

        | FunE(_) -> failwith "function is expected to return a distribution, but returns an expression instead"
        | FunV(_) -> failwith "function is expected to return a distribution, but returns void instead"
    
    | d -> Skip, d 

/// Transforms a statement to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated;
/// TODO: need to make sure that the free vars in their 
/// elaborated versions are also dealt with.
and elaborate_S (defs: FunDef list ) (env: Env) (s: S) : S =
    match s with 
    | DataDecl(t,str, s) -> 
        let newenv = Set.add ((t, Data),str) env
        let s' = elaborate_S defs newenv s
        Block(((t, Data),str), (DataDecl(t,str,s')))

    | Seq(s1, s2) -> 
        let b1 = elaborate_S defs env s1
        let b2 = elaborate_S defs env s2
        match b1,b2 with 
        | Block(env1, s1'), Block(env2, s2') ->             
            Block(env1, Block(env2, Seq(s1', s2')))
        | _ -> Seq(b1, b2)

    | Assign(x, e) -> 
        let b, e' = elaborate_E defs env e 
        match b with 
        | Block(env', s') -> Block(env', Seq(s', Assign(x, e')))
        | _ -> Assign(x, e')

    | Sample(x, d) -> 
        let b, d' = elaborate_D defs env d 
        match b with 
        | Block(env', s') -> Block(env', Seq(s', Sample(x, d')))
        | _ -> Sample(x, d')

    | VCall(x, Es) -> 
        let f = get_fun x defs
        match f with 
        | FunV(_, args, s) -> 
            let args', s', ret' = elaborate_F defs env f
            let body = Seq((assign_all args' Es), s')
            
            match ret' with
            | None ->
                block_from_list_env (args', body)
            | _ -> failwith "unexpected"

        | FunE(_) -> failwith "function was not expected to have a return value, but returns an expression instead"
        | FunD(_) -> failwith "function was not expected to have a return value, but returns a distribution instead"

    | Block(env', s') ->
        let enew = Set.add env' env
        Block(env', elaborate_S defs enew s')

    | s' -> s'

and elaborate_F (defs: FunDef list) (env: Env) (f: FunDef) =
    let args, s, ret = match f with
                       | FunE(_, args, s, ret) -> args, s, ERet(ret)
                       | FunD(_, args, s, ret) -> args, s, DRet(ret)
                       | FunV(_, args, s) -> args, s, None
   
    let locals = get_locals s

    let dict = create_dict env args locals
    let args' = args
             |> List.map (fun (t,v) -> t, Map.safeFind v dict)  

    let s_renamed = rename_S dict s
    let s' = elaborate_S defs (set args') s_renamed

    let ret' = 
        match ret with
        | ERet(r) -> 
            let ret_renamed = rename_E dict r
            let _, r' = elaborate_E defs (set args') ret_renamed
            ERet(r')
        | DRet(r) -> 
            let ret_renamed = rename_D dict r
            let _, r' = elaborate_D defs env ret_renamed
            DRet(r')
        | None -> None
    
    args', s', ret'
 
   

/// Runs basic checks on the function definitions, 
/// to make sure the same variable is not declared 
/// more than once, etc. 
/// FIXME: Will currently break if loops or if-then
/// statements are introduced. 
let rec safetycheck defs =
    let checkdefs name args s =
        let locals = get_locals s
        let all_vars_set = (args@locals)
                        |> List.map (fun (t,v) -> v)
                        |> set

        if (Set.count all_vars_set) < (List.length (args@locals))
        then failwith (sprintf "Variable defined more than once in %A" name)
        else 0

    match defs with
    | [] -> 0
    | d::ds -> 

        let name, args, s = match d with
                            | FunE(name, args, s, ret) -> name, args, s
                            | FunD(name, args, s, ret) -> name, args, s
                            | FunV(name, args, s) -> name, args, s

        checkdefs name args s

         

let elaborate_NewStanProg (prog: NewStanProg) : S =
    match prog with
    | defs, s -> 
        let _ = safetycheck defs
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