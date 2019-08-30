module Factorgraph

open SlicStanSyntax
open Util
open Typecheck
open System.IO

type VarId = string
type FactorId = int 

type VarNode = Type * VarId  
type FactorNode =  S * FactorId 
type Edge = Ev of VarId * FactorId | Ef of FactorId * VarId

type Graph = VarNode list * FactorNode list * Edge list

type NodeId = Vid of VarId | Fid of FactorId

let mutable ordering : VarId list = List.Empty 

let vars ((V, _, _): Graph) = V |> List.map snd 
let factors ((_, F, _): Graph) = F |> List.map snd 
let edges ((_, _, E): Graph) = E

let empty_graph ((V, F, E) : Graph) =
    List.isEmpty V && List.isEmpty F && List.isEmpty E

let get_variable (v : VarId) ((V, _, _) : Graph) =
    List.find (fun (T, x) -> x = v) V 

let get_factor (f : FactorId) ((_, F, _) : Graph) : S =
    List.find (fun (s, f') -> f = f') F |> fst

let has_factor (f : FactorId) ((_, F, _) : Graph) =
    match List.tryFind (fun (s, f') -> f = f') F with
    | None -> false
    | Some _ -> true

let get_in_edges (n : NodeId) ((_, _, E) : Graph) : Edge list =
    match n with 
    | Vid x -> List.filter (fun e -> match e with Ef (f, v) -> x = v | _ -> false) E
    | Fid i -> List.filter (fun e -> match e with Ev (v, f) -> i = f | _ -> false) E

let get_out_edges (n : NodeId) ((_, _, E) : Graph) : Edge list =
    match n with 
    | Vid x -> List.filter (fun e -> match e with Ev (v, f) -> x = v | _ -> false) E
    | Fid i -> List.filter (fun e -> match e with Ef (f, v) -> i = f | _ -> false) E

// For factor ID generation
let mutable cur = 0
let next_factor_id() =
    cur <- cur + 1
    cur
// **********************//

let mutable res_folder = ""
let set_folder name = res_folder <- name

let reads E = read_exp E |> Set.toList


let is_discrete_parameter W = 
    fun ((tp, tl), x) -> tl = Model && tp <. Int && (Set.contains x W |> not)

let add_factor (S: S) (graph: Graph) =
    
    let rec get_dependencies (S: S) : VarId list * VarId list  =
        match S with
        | Assign(lhs, e) -> 
            let out_var = LValueBaseName lhs
            let in_vars = 
                reads e 
                |> List.append (lhs_to_exp lhs |> reads) 
                |> List.filter (fun v -> v <> out_var)
                |> Set.ofList |> Set.toList

            in_vars, [out_var]

        | Sample(e, d) -> 
            match d with 
            | Dist(_, exps) -> 
                let in_vars = reads (Arr exps) |> Set.ofList |> Set.toList
                let out_vars = reads e |> Set.ofList |> Set.toList
                in_vars, out_vars

        | If _ -> failwith "not yet implemented"
        | For ((T, x), lower, upper, s) -> 
            let in_s, out_s = get_dependencies s
            let in_vars = 
                [ match lower with N _ -> "" | SizeVar x -> x;
                  match upper with N _ -> "" | SizeVar x -> x]
                |> List.append in_s |> Set.ofList
                |> fun set -> if Set.contains x set then Set.remove x set else set
                |> Set.toList
            let out_vars = 
                Set.ofList out_s
                |> fun set -> if Set.contains x set then Set.remove x set else set
                |> Set.toList

            in_vars, out_vars
    
        | Decl _ -> failwith "unexpected"
        | Seq _ -> failwith "unexpected"
        | Skip -> failwith "unexpected"
        
        (*| Message(name, var, s) -> 
            // FIXME: is this enough?
            let out_var = snd name
            [], [out_var]*)

        | Elim(messages, var, s) -> 
            let in_vars = messages
            in_vars, []
       
    let V, F, E = graph

    let fid : FactorId = next_factor_id()
    let f : FactorNode = S, fid

    let in_vars, out_vars = get_dependencies S

    let E' =
        List.map (fun v -> Ev(v, fid)) in_vars 
        |> List.append (List.map (fun v -> Ef(fid, v)) out_vars )
        |> List.append E

    (V, f::F, E'), fid


let to_graph (S: S) : Set<Ide> * Graph = 
    
    let W = assigns_global S

    let rec _to_graph (S: S) (current: Graph) : Graph =
        let V, F, E = current
        match S with 
        | Decl(arg, s) -> 
            _to_graph s ( arg::V, F, E )
        | Seq(s1, s2) -> 
            _to_graph s1 (current) |> _to_graph s2 
        | Skip -> current
        | _ -> add_factor S current |> fst
        
    let V, F, E = _to_graph S ([], [], []) 

    W, (List.rev V, List.rev F, E)


let pp_graph W (G : Graph) =
    
    let V, F, E = G

    let vars = 
        List.map (fun (T, x) -> 
                    if is_discrete_parameter W (T, x) 
                    then sprintf "%s [style=filled, shape=doublecircle, fillcolor=mediumaquamarine]" x 
                    else 
                        if Set.contains x W 
                        then sprintf "%s" x
                        else sprintf "%s [style=filled, color=white]" x) V
        |> fun t -> List.Cons ("\n node [shape=circle, color=black, style=\"\"]", t)
        |> List.fold (fun state el -> state + el + "; ") ""
    let factors =
        List.map (fun (S, n) -> sprintf "%A [label=\"%s\"]" n (SlicStanSyntax.S_pretty "" S)) F
        |> fun t -> List.Cons ("\n node [shape=polygon, sides=4, style=filled, color=grey]", t)
        |> List.fold (fun state el -> state + el + "; ") ""
    let edges = List.map (fun e -> 
                            match e with
                            | Ev(v, f) -> sprintf "%s -> %A" v f
                            | Ef(f, v) -> sprintf "%A -> %s" f v
                         ) E
                |> List.fold (fun state el -> state + el + "; ") ""

    sprintf "digraph { %s\n %s\n %s}" vars factors edges 

let graphviz G id info =
    let W = SofList (G |> fun (_, F, _) -> List.map fst F) |> Util.assigns_global
    let gv_graph = pp_graph W G 
    let path = sprintf "../../../../graphs/%s/" res_folder
    let file_name = sprintf "%d_%s.gv" id info
    Directory.CreateDirectory(path) |> ignore
    File.WriteAllText(path+file_name, gv_graph)

let parents (n : NodeId) (G: Graph) : NodeId list =
    get_in_edges n G 
    |> List.map (fun e -> match e with Ef(f, _) -> Fid f | Ev(v, _) -> Vid v)


let children (n : NodeId) (G: Graph) : NodeId list =
    get_out_edges n G 
    |> List.map (fun e -> match e with Ef(_, v) -> Vid v | Ev(_, f) -> Fid f)


/// <summary>Determines weather or not `child` depends on `parent` in graph G.</summary>
/// <param name="parent">The allegedly grandparent node.</param>
/// <param name="child">The allegedly dependent node.</param>
/// <param name="G">The dependency graph.</param>
/// <returns>True if child depends on parent in G.</returns>
let rec depends_on (parent : NodeId) (child : NodeId) (G: Graph) = 
    parent = child ||
    children parent G
    |> List.filter (fun c -> parents parent G |> List.contains c |> not)
    |> List.exists (fun c -> depends_on c child G)
  
 
let remove (n : NodeId) (G: Graph) =
    
    let V, F, E = G

    match n with
    | Vid x -> 
        List.filter (fun v -> snd v <> x) V,
        F,
        List.filter (fun e -> match e with Ev (v, f) -> v <> x | Ef (f, v) -> v <> x) E
    | Fid i -> 
        V,
        List.filter (fun f -> snd f <> i) F,
        List.filter (fun e -> match e with Ev (v, f) -> f <> i | Ef (f, v) -> f <> i) E


let rec dfs (G : Graph) = 
    let Vs =
        vars G
        |> List.filter (fun v -> List.length (parents (Vid v) G) = 0) 

    let Fs = 
        factors G
        |> List.filter (fun f -> List.length (parents (Fid f) G) = 0)
    
    if List.length Vs + List.length Fs = 0 || empty_graph G then G, Skip
    else 
        let G', S' =
            Fs
            |> List.fold (fun (g, s) f -> 
                            if empty_graph g || (has_factor f g |> not) 
                            then g, s 
                            else
                                let sf = get_factor f g
                                let g' = remove (Fid f) g
                                let g'', s' = dfs g'
                                let s'' = Seq(s, Seq(sf, s'))
                                g'', s''                               
                         ) 
                         (G, Skip)
        let G'', S'' = 
            Vs
            |> List.fold (fun (g, s) v -> 
                            if empty_graph g then g, s else
                                let g' = remove (Vid v) g
                                let g'', s' = dfs g'
                                // FIXME: need to be more careful if we want proper scope
                                // for non-discrete-param variables.
                                let s'' = Seq(s, s') //let s'' = Decl(get_variable v g, Seq(s, s'))
                                g'', s''                               
                         ) 
                         (G', S')                         
        G'', S''


let find_ordering (W : Set<Ide>) (G: Graph) : Ide list = 
    // Use depth as a way to choose ordering

    let V, _, _ = G

    let discrete_params = 
        List.filter (is_discrete_parameter W) V
        |> List.map (fun (_, x) -> x) 
    
    let find_ancestors (x : VarId) : VarId list =
        discrete_params 
        |> List.filter (fun a -> a <> x && depends_on (Vid a) (Vid x) G)

    let ancestors = List.map (fun x -> x, find_ancestors x |> List.length) discrete_params
    
    let order = 
        ancestors
        |> List.sortBy (fun (x, num_ancestors) -> num_ancestors)
        |> List.map fst 

    order //|> List.rev

    

let merge_simple (G : Graph) f1 f2 : (Graph * FactorId) = 
    let s1, s2 = get_factor f1 G , get_factor f2 G
    let new_factor : FactorNode = Seq(s1, s2), next_factor_id()

    let V, F, E = G
    let V' = V
    let F' = new_factor :: F |> List.filter (fun (_, f) -> f <> f1 && f <> f2) 
    let E' = List.map (fun e -> 
                            match e with 
                            | Ev(v, f) -> if f = f1 || f = f2 then Ev(v, snd new_factor) else e
                            | Ef(f, v) -> if f = f1 || f = f2 then Ef(snd new_factor, v) else e
                      ) E 

    (V', F', E'), snd new_factor


let rec merge (G : Graph) (phi : FactorId) (f : FactorId) : (Graph * FactorId) = 
    if phi = f || (depends_on (Fid phi) (Fid f) G |> not) then merge_simple G phi f
    else 
        let next_factors =
            children (Fid phi) G
            |> List.filter (fun n -> match n with Vid v -> depends_on (Vid v) (Fid f) G) 
            |> List.fold (fun list ch -> 
                            children ch G
                            |> List.filter (fun n -> match n with Fid i -> i <> phi && depends_on (Fid i) (Fid f) G)
                            |> List.append list
                         ) [] 
            |> List.map (fun n -> match n with Fid i -> i)

        if next_factors = [f] then merge_simple G phi f
        else        
            let G', phi' = List.fold (fun (g, new_f) f' -> merge_simple g new_f f') (G, phi) next_factors
            merge G' phi' f 


let merge_many (factors : FactorId list) (G : Graph) : Graph * FactorId =
    let factors_head, factors_tail = 
        match factors with
        | [] -> -1, []
        | head::tail -> head, tail 
    
    List.fold ( fun (g, phi) f -> merge g phi f )
                  (G, factors_head) factors_tail


let rec merge_dependent (factors : FactorId list) (G : Graph) : Graph * FactorId = 
    // when doing variable elimination, we can't just merge the 
    // neighbouring factors; need to merge all dependent factors
    
    let fs = List.filter (fun f -> f <> -1) factors
    if fs = [] then G, -1
    else 
        let G', f' = merge_many fs G
        let var_children = children (Fid f') G'
                
        assert(
            var_children 
            |> List.map (fun c -> match c with Vid x -> x) 
            |> Set.ofList 
            |> Collections.Set.intersect (Set.ofList ordering)
            |> Set.isEmpty
        )

        let children = 
            var_children
            |> List.fold (fun list v -> 
                            children v G'
                            |> List.map (fun n -> match n with Fid f -> f)
                            |> List.filter (fun f -> children (Fid f) G' |> List.contains v |> not)
                            |> List.append list
                         ) []
            |> List.filter (fun f -> f <> -1)

        if children = [] then G', f'
        else 
            let G'', f'' = merge_dependent children G'
            merge_simple G'' f' f''
        
    


let update_factor (update_function : S -> S) (factor_id : FactorId) (G : Graph) : Graph =
    let V, F, E = G
    let s' = get_factor factor_id G |> update_function    
    V, List.map (fun (s, f) -> if f = factor_id then (s', f) else (s, f)) F, E

/// Eliminates variable d in graph G.
/// Returns the updated graph and the factor resulting in the elimination of d.
let eliminate (G: Graph) (d: VarId)  = 
    
    let parents_d = parents (Vid d) G |> List.map (fun n -> match n with Fid f -> f)
    let children_d = children (Vid d) G |> List.map (fun n -> match n with Fid f -> f)

    //assert(List.allPairs parents_d children_d |> List.exists (fun (p, c) -> depends_on (Fid c) (Fid p) G) |> not)

    let G', in_factor = merge_many parents_d G
    let G'', out_factor = merge_dependent children_d G'

    let G_res, d_factor_res = 
        match in_factor, out_factor with 
        | -1, -1 -> failwith "unexpected"  
        | -1, _ -> G'', out_factor
        | _, -1 -> G', in_factor
        | _, _ -> merge_simple G'' in_factor out_factor

    let decl = get_variable d G
    G_res |> update_factor (fun s -> Decl(decl, s)) d_factor_res |> remove (Vid d)


let direct (G: Graph) : Graph =
    let V, F, E = G
    let E' = List.filter (fun e -> match e with Ef(f, v) -> List.contains (Ev(v, f)) E |> not | _ -> true) E

    V, F, E'

let eliminate_variables (graph : Graph) (order : Ide list) : S =
    // Use the ordering to eliminate the discrete variables 
    // one by one. Merge the resulting graph in a single statement.

    ordering <- order

    let interm = List.fold eliminate graph order
    let vs, _, _ = interm

    interm 
    |> direct 
    |> dfs |> snd 
    |> DeclOfList vs



