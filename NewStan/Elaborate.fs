module Elaborate

open SlicStanSyntax
open System.Runtime.CompilerServices

type Dict = Map<Ide,Ide>   // Gamma
type Context = Set.Context // Gamma

type Ret = ERet of Exp | DRet of Dist | Unit 

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

    let rec _assign (tuples: (Arg*Exp) list ) =
        match tuples with
        | [] -> Skip
        | ((t,v), r)::zs -> Seq(Assign(I(v),r), _assign zs)

    _assign zipped


/// Creates a nested blick statement S from 
/// a list of the veriables in scope (env), 
/// and an inner statement (body).
/// Example: block_from_list_env ({x,y}, S1) 
/// Returns: Block(x, Block(y, S1)).
let rec block_from_list_env (env: List<Arg>, body) =    
    List.fold (fun s v -> Decl(v, s)) body env 


let create_dict (mainC: Context) (secondaryC: Context) :Dict =
    let prime (v:Ide) = 
        v + "p"

    let rec fresh (C: Context) (v:Ide) =
        if Set.fold (fun s (t,name) -> s || (v=name)) false C 
        then fresh C (prime v)
        else v

    let clashes = Set.intersectNames mainC secondaryC
    let allC = Set.union mainC secondaryC    
    let d = Set.map (fun x -> x, fresh allC x) clashes

    Map.ofList (Set.toList d)
        

/// Renames the single variable arg, as specified by dict.
let rec rename_arg (dict:Dict) (arg: Arg):Arg =
    match arg with t, x -> t, Map.safeFind x dict

let rename_args (dict:Dict) (args: Arg list) =
    List.map (rename_arg dict) args

let rename_Ctx (dict: Dict) (C: Context): Context =
    Set.map (fun (t,x) -> (t, Map.safeFind x dict)) C

/// Renames all bound variables in e, as specified by dict.
let rec rename_E (dict:Dict) (e: Exp):Exp =
    match e with
    | Var(v) ->  Var(Map.safeFind v dict)
    | Arr(Es) -> Arr(List.map (rename_E dict) Es)
    | ArrElExp(e1, e2) -> ArrElExp(rename_E dict e1, rename_E dict e2)
    | Const(n) -> Const(n) 
    | Plus(e1, e2) -> Plus(rename_E dict e1, rename_E dict e2)
    | Mul(e1, e2) -> Mul(rename_E dict e1, rename_E dict e2)
    | Prim(name, Es) -> Prim(name, List.map (rename_E dict) Es)
    | ECall(name, Es) -> ECall(name, List.map (rename_E dict) Es)

/// Renames all bound variables in d, as specified by dict.
let rec rename_D (dict:Dict) (d: Dist):Dist =
    match d with
    | Dist(name, Es) -> Dist(name, List.map (rename_E dict) Es)

let rec rename_LValue (dict:Dict) (lhs:LValue): LValue =
    match lhs with
    | I(name) -> I(Map.safeFind name dict)
    | A(lhs', e) -> A(rename_LValue dict lhs', rename_E dict e)


/// Renames all bound variables in s, as specified by dict.
let rec rename_S (dict:Dict) (s: S):S =
    match s with
    //| DataDecl(t, x, s') -> DataDecl(t, Map.safeFind x dict, rename_S dict s')
    | Decl(env, s') -> Decl(rename_arg dict env, rename_S dict s')
    | Sample(e, d) -> Sample(rename_E dict e, rename_D dict d)
    | Assign(lhs, e) -> Assign(rename_LValue dict lhs, rename_E dict e)
    | If(e, s1, s2) -> If(rename_E dict e, rename_S dict s1, rename_S dict s2)
    | Seq(s1, s2) -> Seq(rename_S dict s1, rename_S dict s2)
    | Skip -> Skip
  

let rec getType_Exp (ctx: Context) (e: Exp): TypePrim =
    let ty x: TypePrim = 
        // FIXME: implement this properly
        Real

    let signatures = Primitives
    
    match e with
    | Var(x) -> 
        if Set.contextContains x ctx then Set.contextItemTypePrim x ctx
        else failwith (sprintf "%s not found in type environment" x)

    | Const(n) -> ty(n)

    | Arr(Es) -> 
        let types = List.map (getType_Exp ctx) Es         
        Array(List.head types, N (List.length types))

    | ArrElExp(e1, e2) -> 
        let tau1 = getType_Exp ctx e1
        match tau1 with 
        | Array(t, n) -> t
        | Vector(n) -> Real
        | Matrix(n1, n2) -> Vector(n1)
        | _ -> failwith "unexpected type error"

    | Prim(name, Es) -> 
        let types =  List.map (getType_Exp ctx) Es 

        let fun_signature = 
            if signatures.ContainsKey name then
                signatures.Item name
            else failwith (sprintf "function %s not defined" name)
            
        let taus, tau = fun_signature
        tau


    | Plus(e1, e2) -> getType_Exp ctx (Prim("+", [e1; e2]))
    | Mul(e1, e2) -> getType_Exp ctx (Prim("*", [e1; e2]))

let resolve_S (c:Context) (cs:Context) (s:S) =
    if Set.intersectEmpty c cs then cs, s
    else failwith "resolve_S not implemented"


let fv (e: Exp): (Context) =

    let rec _fv e acc =    
        match e with 
        //here is where the problem is... we lost information about e's type.

        | Var(v) -> Set.add ((Real, Data), v) acc //FIXME: needs to return actual type
        | Const(n) -> acc 
        | Arr(Es) -> List.fold (fun sacc e -> _fv e sacc) acc Es
        | ArrElExp(e1, e2) ->  _fv e1 (_fv e2 acc)
        | Plus(e1, e2) -> _fv e1 (_fv e2 acc)
        | Mul(e1, e2) -> _fv e1 (_fv e2 acc)
        | Prim(name, Es) -> List.fold (fun sacc e -> _fv e sacc) acc Es
        | ECall(name, Es) -> failwith("unexpected")

    _fv e empty



/// Given a list 'all' of Context-Statement-Expression
/// tripples, the function goes through each consequitive
/// pair of contexts, checking if there is a clash;
/// If there is it renames the latter context and its
/// associated S and Exp, so no name clashes occur. 
/// It accumulates the result in the union of all 
/// (renamed) context, and lists of renamed S-s and Exp-s.
let tripple_Rename_and_Fold (all: (Context*S*Exp) list) =
    let c, ss, es = all |> List.fold 
                            (fun (cprev, sprev, esprev) (cc, sc, ec) ->                     
                                if Set.intersectEmpty cprev cc then            
                                    ((Set.union cprev cc), sc::sprev, ec::esprev)
                                else 
                                    let dict = create_dict cprev cc
                                    let cc', sc', ec' = (rename_Ctx dict cc), (rename_S dict sc), (rename_E dict ec)
                                    ((Set.union cprev cc'), sc'::sprev, ec'::esprev)
                                ) (empty, [], [])
    c, (List.rev ss), (List.rev es)
    

let adjust_types (ctx: Context) (args: Arg list) (types: TypePrim list) : Context =
    
    let _, names = List.unzip args
    let actuals = Map.ofList (List.zip names types)

    let _adjust (((t, l),name): Type*Ide) =
        if actuals.ContainsKey name then
            let actual = actuals.Item(name)
            ((actual, l), name)
        else ((t, l),name)

    Set.map _adjust ctx  

/// Transforms an expression to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
let rec elaborate_E (defs: FunDef list) (exp: Exp) : Context*S*Exp =
    match exp with 
    // TODO: need to include user-defined functions again
    (*| ECall(x, Es) -> 
        let f = get_fun x defs
        match f with 
        | FunE(_, args, s, ret) ->             
            let all_es = List.map (elaborate_E defs) Es
            let ces, ss, es = tripple_Rename_and_Fold all_es

            let fvs = Set.unionMany (List.map (fun e -> (fv e)) es)
            let ces_all = Set.union fvs ces

            let argsf, cf_all, sf, ERet(ef) = elaborate_F defs f

            let body, all, ef' = 
                if Set.intersectEmpty ces_all cf_all then
                    let body = Seq(SofList ss, Seq((assign_all argsf es), sf))
                    let all = Set.union (ces) cf_all
                    body, all, ef
                else 
                    let dict = create_dict ces_all cf_all 
                    let cf_all', sf', ef' = (rename_Ctx dict cf_all), (rename_S dict sf), (rename_E dict ef)                    
                    let argsf' = rename_args dict argsf

                    // this next line is what doesn't work --- 
                    // I don't have enough type context to figure out 
                    // the types of expressions at this point
                    let types = (List.map (getType_Exp ces_all) es) 
                    let cf_all'' = adjust_types cf_all' argsf' types
                    let body = Seq(SofList ss, Seq((assign_all argsf' es), sf'))
                    let all = Set.union (ces) cf_all''
                    body, all, ef'

            all, body, ef' *)

    | Plus(E1, E2) -> 
        let c1, s1, e1 = elaborate_E defs E1
        let c2, s2, e2 = elaborate_E defs E2
        if Set.intersectEmpty c1 c2 then            
            (Set.union c1 c2), Seq(s1, s2), Plus(e1, e2)
        else 
            let dict = create_dict c1 c2 
            let c2', s2', e2' = (rename_Ctx dict c2), (rename_S dict s2), (rename_E dict e2)
            (Set.union c1 c2'), Seq(s1, s2'), Plus(e1, e2')

    | Mul(E1, E2) ->
        let c1, s1, e1 = elaborate_E defs E1
        let c2, s2, e2 = elaborate_E defs E2
        if Set.intersectEmpty c1 c2 then            
            (Set.union c1 c2), Seq(s1, s2), Mul(e1, e2)
        else 
            let dict = create_dict c1 c2 
            let c2', s2', e2' = (rename_Ctx dict c2), (rename_S dict s2), (rename_E dict e2)
            (Set.union c1 c2'), Seq(s1, s2'), Mul(e1, e2')
    | Prim(name, Es) ->
                        
        let all = List.map (elaborate_E defs) Es 
        
        let c, ss, es = tripple_Rename_and_Fold all        
        c, SofList ss, Prim(name, es)

    | e -> empty, Skip, e 

/// Transforms a distribution to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
and elaborate_D (defs: FunDef list) (dist: Dist) : Context*S*Dist =
    match dist with
    | Dist(name, Es) -> 
        let all = List.map (elaborate_E defs) Es 

        let c, ss, es = tripple_Rename_and_Fold all        
        c, SofList ss, Dist(name, es)

/// Transforms a statement to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated;
/// TODO: need to make sure that the free vars in their 
/// elaborated versions are also dealt with.
and elaborate_S (defs: FunDef list ) (s: S) : Context*S =
    match s with 
    | Decl((T,x), s) ->
        let c, s' = elaborate_S defs s
        if Set.contextContains x c then
            let dict = create_dict (Set.singleton x) c 
            let c', s'' = (rename_Ctx dict c), (rename_S dict s')
            (Set.add (T,x) c'), s''
        else
            let c' = Set.add (T,x) c 
            c', s'    

    | Seq(s1, s2) -> 
        let c1, s1' = elaborate_S defs s1
        let c2, s2' = elaborate_S defs s2

        if Set.intersectEmpty c1 c2 then
            Set.union c1 c2, Seq(s1', s2')
        else 
            let dict = create_dict c1 c2
            let c2', s2'' = (rename_Ctx dict c2), (rename_S dict s2')
            Set.union c1 c2', Seq(s1', s2'')

    | Assign(lhs, e) -> 
        let c, s, e' = elaborate_E defs e 
        let x = LValueBaseName lhs       
        if Set.contextContains x c then
            let dict = create_dict (Set.singleton x) c // FIXME: need to include other names that lhs might have
            let c', s', e'' = (rename_Ctx dict c), (rename_S dict s), (rename_E dict e')
            c', Seq(s', Assign(lhs, e''))
        else c, Seq(s, Assign(lhs, e'))

    | Sample(e, d) -> 
        
        let ce, se, e' = elaborate_E defs e
        let cd, sd, d' = elaborate_D defs d 

        if Set.intersectEmpty ce cd then 
            Set.union ce cd, Seq(se, Seq(sd, Sample(e', d')))
        else 
           let dict = create_dict ce cd
           let cd', sd', d'' = (rename_Ctx dict cd), (rename_S dict sd), (rename_D dict d')
           Set.union ce cd', Seq(se, Seq(sd', Sample(e', d'')))

    | If(e, s1, s2) ->
        let ce, se, e' = elaborate_E defs e
        let cs1, s1' = elaborate_S defs s1
        let cs2, s2' = elaborate_S defs s2

        if Set.intersectEmpty ce cs1 && Set.intersectEmpty ce cs2 then
            Set.union ce cs1 |> Set.union cs2, If(e', s1', s2')
        else 
            let dict = create_dict ce (Set.union cs1 cs2)
            let cs1', s1'' = (rename_Ctx dict cs1), (rename_S dict s1')
            let cs2', s2'' = (rename_Ctx dict cs2), (rename_S dict s2')
            Set.union ce cs1' |> Set.union cs2', If(e', s1'', s2'')

    (*| VCall(x, Es) -> 
        let f = get_fun x defs
        match f with 
        | FunV(_, args, s, _) -> 

            let all_es = List.map (elaborate_E defs) Es
            let ces, ss, es = tripple_Rename_and_Fold all_es

            let fvs = Set.unionMany (List.map (fun e -> (fv(e))) es)
            let ces_all = Set.union fvs ces

            let argsf, cf_all, sf, Unit = elaborate_F defs f

            let body, all = 
                if Set.intersectEmpty ces_all cf_all then
                    let body = Seq(SofList ss, Seq((assign_all argsf es), sf))
                    let all = Set.union (ces) cf_all
                    body, all
                else 
                    let dict = create_dict ces_all cf_all 
                    let cf_all', sf' = (rename_Ctx dict cf_all), (rename_S dict sf)
                    let argsf' = rename_args dict argsf
                    let body = Seq(SofList ss, Seq((assign_all argsf' es), sf'))
                    let all = Set.union (ces) cf_all'
                    body, all

            all, body *)
                
    | Skip -> empty, Skip

and elaborate_F (defs: FunDef list) (f: FunDef) =
    
    failwith "need to implement elaboration of user-defined functions"
    (*
    let args, s, ret = match f with Fun(_, args, s, ret) -> args, s, ERet(ret)
   
    let cs, ss = elaborate_S defs s

    let args' = if (Set.intersectEmpty (set args) cs)
                then args
                else failwith "not implemented"

    let ce, se, ret = 
        match ret with
        | ERet(r) -> 
            let c, s, r' = elaborate_E defs r
            let c', s' = resolve_S (Set.union (set args) cs) c s
            c', s', ERet(r')
        | DRet(r) -> 
            let c, s, r' = elaborate_D defs r
            let c', s' = resolve_S (Set.union (set args) cs) c s
            c', s', DRet(r')
        | Unit ->
            empty, Skip, Unit  
    
    args', (Set.union (Set.union cs ce) (Set.ofList args')), (Seq(ss, se)), ret *)
   

/// Runs basic checks on the function definitions, 
/// to make sure the same variable is not declared 
/// more than once, etc. 
/// FIXME: Will currently break if loops or if-then
/// statements are introduced. 
let rec safetycheck defs =
    let get_locals (s: S) : List<Arg> =
        let rec rec_locals s acc = 
            match s with
            | Decl(env, s') ->  rec_locals s' (env::acc)
            | Seq(s1, s2) -> rec_locals s1 (rec_locals s2 acc)
            | _ -> acc

        rec_locals s []

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
                            | Fun(name, args, s, ret) -> name, args, s

        checkdefs name args s

         

let elaborate_SlicStanProg (prog: SlicStanProg) : Context*S =
    match prog with
    | defs, s -> 
        let _ = safetycheck defs
        elaborate_S defs s