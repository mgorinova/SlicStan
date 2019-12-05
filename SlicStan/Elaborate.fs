module Elaborate

open SlicStanSyntax
open System.Runtime.CompilerServices
open Util 

type Dict = Map<Ide,Ide>   // Gamma
type Gamma = Map.Gamma // Gamma

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


let create_dict (mainC: Gamma) (secondaryC: Gamma) :Dict =
    let prime (v:Ide) = 
        v + "p"

    let rec fresh (C: Gamma) (v:Ide) =
        if Map.fold (fun s name t -> s || (v = name)) false C 
        then fresh C (prime v)
        else v

    let clashes = Map.intersectNames mainC secondaryC
    let allC = Map.union mainC secondaryC    
    let d = Set.map (fun x -> x, fresh allC x) clashes

    Map.ofList (Set.toList d)
        

/// Renames the single variable arg, as specified by dict.
let rec rename_arg (dict:Dict) (arg: Arg):Arg =
    match arg with t, x -> t, Map.safeFind x dict

let rename_args (dict:Dict) (args: Arg list) =
    List.map (rename_arg dict) args

let rename_Ctx (dict: Dict) (g: Gamma): Gamma =
    Map.toList g
    |> List.map (fun (x, t) -> (Map.safeFind x dict), t)
    |> Map.ofList

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
    | Decl(env, s') -> Decl(rename_arg dict env, rename_S dict s')
    | Sample(e, d) -> Sample(rename_E dict e, rename_D dict d)
    | Assign(lhs, e) -> Assign(rename_LValue dict lhs, rename_E dict e)
    | If(e, s1, s2) -> If(rename_E dict e, rename_S dict s1, rename_S dict s2)
    // FIXME: array sizes might need to be renamed too 
    | For(x, lower, upper, s) -> For(rename_arg dict x, lower, upper, rename_S dict s) 
    | Seq(s1, s2) -> Seq(rename_S dict s1, rename_S dict s2)
    | Skip -> Skip
  

let rec getType_Exp (ctx: Gamma) (e: Exp): TypePrim =
    let ty x: TypePrim = 
        // FIXME: implement this properly
        Real

    let signatures = Primitives
    
    match e with
    | Var(x) -> 
        if Map.gammaContains x ctx then Map.gammaItemTypePrim x ctx
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

    | ECall(_) -> failwith "not supported"

let resolve_S (c:Gamma) (cs:Gamma) (s:S) =
    if Map.intersectEmpty c cs then cs, s
    else failwith "resolve_S not implemented"


let fv (e: Exp): (Gamma) =

    let rec _fv e acc =    
        match e with 
        //here is where the problem is... we lost information about e's type.

        | Var(v) -> Map.add v (Real, Data) acc //FIXME: needs to return actual type
        | Const(n) -> acc 
        | Arr(Es) -> List.fold (fun sacc e -> _fv e sacc) acc Es
        | ArrElExp(e1, e2) ->  _fv e1 (_fv e2 acc)
        | Plus(e1, e2) -> _fv e1 (_fv e2 acc)
        | Mul(e1, e2) -> _fv e1 (_fv e2 acc)
        | Prim(name, Es) -> List.fold (fun sacc e -> _fv e sacc) acc Es
        | ECall(name, Es) -> failwith("unexpected")

    _fv e empty



/// Given a list 'all' of Gamma-Statement-Expression
/// tripples, the function goes through each consequitive
/// pair of gammas, checking if there is a clash;
/// If there is it renames the latter gamma and its
/// associated S and Exp, so no name clashes occur. 
/// It accumulates the result in the union of all 
/// (renamed) gamma, and lists of renamed S-s and Exp-s.
let tripple_Rename_and_Fold (all: (Gamma*S*Exp) list) =
    let c, ss, es = all |> List.fold 
                            (fun (cprev, sprev, esprev) (cc, sc, ec) ->                     
                                if Map.intersectEmpty cprev cc then            
                                    ((Map.union cprev cc), sc::sprev, ec::esprev)
                                else 
                                    let dict = create_dict cprev cc
                                    let cc', sc', ec' = (rename_Ctx dict cc), (rename_S dict sc), (rename_E dict ec)
                                    ((Map.union cprev cc'), sc'::sprev, ec'::esprev)
                                ) (empty, [], [])
    c, (List.rev ss), (List.rev es)
    

let adjust_types (g: Gamma) (args: Arg list) (types: TypePrim list) : Gamma =
    
    let _, names = List.unzip args
    let actuals = Map.ofList (List.zip names types)

    let _adjust (name : Ide) ((t, l) : Type) : Type =
        if actuals.ContainsKey name then
            let actual = actuals.Item(name)
            (actual, l)
        else (t, l)

    Map.map _adjust g 

/// Transforms an expression to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
let rec elaborate_E (defs: FunDef list) (exp: Exp) : Gamma*S*Exp =
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
                    // I don't have enough type gamma to figure out 
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
        if Map.intersectEmpty c1 c2 then            
            (Map.union c1 c2), Seq(s1, s2), Plus(e1, e2)
        else 
            let dict = create_dict c1 c2 
            let c2', s2', e2' = (rename_Ctx dict c2), (rename_S dict s2), (rename_E dict e2)
            (Map.union c1 c2'), Seq(s1, s2'), Plus(e1, e2')

    | Mul(E1, E2) ->
        let c1, s1, e1 = elaborate_E defs E1
        let c2, s2, e2 = elaborate_E defs E2
        if Map.intersectEmpty c1 c2 then            
            (Map.union c1 c2), Seq(s1, s2), Mul(e1, e2)
        else 
            let dict = create_dict c1 c2 
            let c2', s2', e2' = (rename_Ctx dict c2), (rename_S dict s2), (rename_E dict e2)
            (Map.union c1 c2'), Seq(s1, s2'), Mul(e1, e2')
    | Prim(name, Es) ->
                        
        let all = List.map (elaborate_E defs) Es 
        
        let c, ss, es = tripple_Rename_and_Fold all        
        c, SofList ss, Prim(name, es)

    | e -> empty, Skip, e 

/// Transforms a distribution to a block and a return value.
/// TODO: make sure the arguments, in the case when the
/// input expression is a function, are also elaborated.
and elaborate_D (defs: FunDef list) (dist: Dist) : Gamma*S*Dist =
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
and elaborate_S (defs: FunDef list ) (s: S) : Gamma * S =
    match s with 
    | Decl((T,x), s) ->
        let g, s' = elaborate_S defs s
        if Map.gammaContains x g then
            let dict = create_dict (Map.singleton x) g 
            let g', s'' = (rename_Ctx dict g), (rename_S dict s')
            (Map.add x T g'), s''
        else
            let g' = Map.add x T g 
            g', s'    

    | Seq(s1, s2) -> 
        let g1, s1' = elaborate_S defs s1
        let g2, s2' = elaborate_S defs s2

        if Map.intersectEmpty g1 g2 then
            Map.union g1 g2, Seq(s1', s2')
        else 
            let dict = create_dict g1 g2
            let g2', s2'' = (rename_Ctx dict g2), (rename_S dict s2')
            Map.union g1 g2', Seq(s1', s2'')

    | Assign(lhs, e) -> 
        let g, s, e' = elaborate_E defs e 
        let x = LValueBaseName lhs       
        if Map.gammaContains x g then
            let dict = create_dict (Map.singleton x) g // FIXME: need to include other names that lhs might have
            let g', s', e'' = (rename_Ctx dict g), (rename_S dict s), (rename_E dict e')
            g', Seq(s', Assign(lhs, e''))
        else g, Seq(s, Assign(lhs, e'))
        
    | Sample(e, d) -> 
        
        let ge, se, e' = elaborate_E defs e
        let gd, sd, d' = elaborate_D defs d 

        if Map.intersectEmpty ge gd then 
            Map.union ge gd, Seq(se, Seq(sd, Sample(e', d')))
        else 
           let dict = create_dict ge gd
           let gd', sd', d'' = (rename_Ctx dict gd), (rename_S dict sd), (rename_D dict d')
           Map.union ge gd', Seq(se, Seq(sd', Sample(e', d'')))

    | If(e, s1, s2) ->
        let ge, se, e' = elaborate_E defs e
        let gs1, s1' = elaborate_S defs s1
        let gs2, s2' = elaborate_S defs s2

        if Map.intersectEmpty ge gs1 && Map.intersectEmpty ge gs2 then
            Map.union ge gs1 |> Map.union gs2, If(e', s1', s2')
        else 
            let dict = create_dict ge (Map.union gs1 gs2)
            let gs1', s1'' = (rename_Ctx dict gs1), (rename_S dict s1')
            let gs2', s2'' = (rename_Ctx dict gs2), (rename_S dict s2')
            Map.union ge gs1' |> Map.union gs2', If(e', s1'', s2'')

    | For((T, x), lower, upper, s) -> 
    
        let c, s' = elaborate_S defs s
        
        if Map.gammaContains x c then
            let dict = create_dict (Map.singleton x) c 
            let c', s'' = (rename_Ctx dict c), (rename_S dict s')
            c', For((T, x), lower, upper, s'')
        else
            c, For((T, x), lower, upper, s') 
             
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

         
         
let elaborate_Prog (prog: SlicStanProg) : Gamma * S =
    match prog with
    | defs, s -> 
        let _ = safetycheck defs
        elaborate_S defs s