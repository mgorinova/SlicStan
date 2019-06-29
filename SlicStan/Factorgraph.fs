module Factorgraph

open SlicStanSyntax 
open Util

type Marginalisation = bool

type VarNode = Arg  
type FactorNode =  int * S

type VarAdj = int list 
type FactorAdj = Ide list 

// in edges -> vriable -> out edges
type VarContext = VarAdj * VarNode * VarAdj
type FactorContext = FactorAdj * FactorNode * FactorAdj

type Graph = VarContext list * FactorContext list


let mutable cur = 0
let next_factor_id() =
    cur <- cur + 1
    cur

let rec reads (E: Exp) : Ide list =
    match E with 
    | Const _ -> []
    | Var(x) -> [x]
    | Arr(exps) -> 
        let folder = fun current e -> List.append (reads e) current
        List.fold folder [] exps
    | ArrElExp (e1, e2) -> List.append (reads e1) (reads e2)
    | Plus (e1, e2) -> List.append (reads e1) (reads e2)
    | Mul (e1, e2) -> List.append (reads e1) (reads e2)
    | Prim (_, exps) ->
        let folder = fun current e -> List.append (reads e) current
        List.fold folder [] exps
    | ECall _ -> failwith "unexpected"


let add_factor (S: S) (graph: Graph) = 
    
    let n = next_factor_id()
    let new_factor : FactorNode = n, S

    let add_in_factor_to_vars (vars: Ide list) (graph: Graph) =
        let varctx, factorctx = graph
        
        let rec add_to_f (factorctx : FactorContext list): FactorContext list = 
            match factorctx with
            | [] -> [ [], new_factor, vars ]
            | (inlist, (id, s), outlist) :: tail -> 
                if n = id then 
                    (inlist, (id, s), List.append vars outlist |> Set.ofList |> Set.toList) :: tail
                else (inlist, (id, s), outlist) :: (add_to_f tail)

        let rec add_to_v (varctx : VarContext list): VarContext list = 
            match varctx with
            | [] -> []
            | (inlist, (T, x), outlist) :: tail -> 
                if List.contains x vars then (n :: inlist, (T, x), outlist) :: (add_to_v tail)
                else (inlist, (T, x), outlist) :: (add_to_v tail)

         
        let new_factorctx = add_to_f factorctx
        let new_varctx = add_to_v varctx

        new_varctx, new_factorctx

    let add_out_factor_to_vars (vars: Ide list) (graph: Graph) =
        
        let varctx, factorctx = graph
        
        let rec add_to_f (factorctx : FactorContext list): FactorContext list = 
            match factorctx with
            | [] -> [ vars, new_factor, [] ]
            | (inlist, (id, s), outlist) :: tail -> 
                if n = id then 
                    (List.append vars inlist |> Set.ofList |> Set.toList, (id, s), outlist) :: tail
                else (inlist, (id, s), outlist) :: (add_to_f tail)

        let rec add_to_v (varctx : VarContext list): VarContext list = 
            match varctx with
            | [] -> []
            | (inlist, (T, x), outlist) :: tail -> 
                if List.contains x vars then (inlist, (T, x), n :: outlist) :: (add_to_v tail)
                else (inlist, (T, x), outlist) :: (add_to_v tail)

         
        let new_factorctx = add_to_f factorctx
        let new_varctx = add_to_v varctx

        new_varctx, new_factorctx

    match S with
    | Assign(lhs, e) -> 
        let involved_vars = reads e
        graph |> add_in_factor_to_vars [Util.LValueBaseName lhs] 
              |> add_out_factor_to_vars involved_vars

    | Sample(e, d) -> 
        match d with 
        | Dist(_, exps) ->
            let involved_vars = reads (Arr exps)
            graph |> add_in_factor_to_vars (Typecheck.read_exp e |> Set.toList)
                  |> add_out_factor_to_vars involved_vars
        | _ -> failwith "unexpected"

    | If _ -> failwith "not yet implemented"
    | For _ -> failwith "not yet implemented"
    
    | Decl _ -> failwith "unexpected"
    | Seq _ -> failwith "unexpected"
    | Skip -> failwith "unexpected"
    

let to_graph (S: S) : Graph =
    
    let rec _to_graph (S: S) (current: Graph) : Graph =
        let varctx, factorctx = current
        match S with 
        | Decl(arg, s) -> 
            _to_graph s ( (VarContext([], arg, []) :: varctx),  factorctx)
        | Seq(s1, s2) -> 
            _to_graph s1 (current) |> _to_graph s2 
        | Skip -> current
        | _ -> add_factor S current

    _to_graph S ([], [])
    

let pp_graph ((vc, fc): Graph) : string =
    
    let pp_vcontext (vcontext: VarContext) : string =
        let adj1, (T, x), adj2 = vcontext
        sprintf "%A --> %s %s --> %A\n" adj1 (Type_pretty T) x adj2 

    let pp_fcontext (fcontext: FactorContext) : string =
        let adj1, (n, s), adj2 = fcontext
        sprintf "%A --> %A: %s --> %A\n" adj1 n (S_pretty "" s) adj2 

    List.map pp_vcontext vc 
    |> List.append (List.map pp_fcontext fc) 
    |> List.fold (fun acc e -> acc + e) "" 