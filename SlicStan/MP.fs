module MP
open Factorgraph
open SlicStanSyntax
open Util

let messageNames = Seq.initInfinite (fun index ->
        "mu_" + (string index) )

let mutable message_name_id = 0
let next_message_id() =
    let ret = Seq.item message_name_id messageNames
    message_name_id <- message_name_id + 1
    ret
let reset_messages() = message_name_id <- 0

let enum_type T : Type=
    let tp, tl = T
    match tp with 
    | Constrained(t, n) -> Vector(n), Model
    | _ -> failwith "Unexpected! Tried to eliminate a variable with unknown support."

let messagify (d : Arg) (G: Graph) (f: FactorId) : Graph * Ide =
    let rec _messagify m S =
        match S with
        //| Seq(s1, s2) -> Seq(_messagify m s1, _messagify m s2)
        //| Message(name, x, s) -> Message(name, x, s)
        | _ -> 
            let arg, v = m
            Message(arg, v, S)

    let name = next_message_id()
    let arg = (enum_type (fst d), name)
    let update = fun s -> _messagify (arg, snd d) s
    let G' = 
        update_factor update f G 
        |> fun (vs, fs, es) -> 
            arg::vs, fs, 
            Ef(f, name) :: es 
            |> List.filter (fun e -> match e with 
                                     | Ef(f', d') -> (d'=snd d && f'=f) |> not 
                                     | Ev(d', f') -> (d'=snd d && f'=f) |> not)
    G', name

let elim (d : VarId) (messages : VarId list) (G: Graph, f: FactorId) =
    let decl = get_variable d G
    if f = -1 then 
        let new_graph, fid = add_factor (Elim(messages, decl, Skip)) G 
        let new_edges = List.map (fun m -> Ev(m, fid)) messages
        (new_graph 
        |> remove (Vid d) 
        |> fun (vs, fs, es) -> vs, fs, List.append new_edges es), fid   
        
    else (G |> update_factor (fun s -> Elim(messages, decl, s)) f 
            |> remove (Vid d)
            |> fun (vs, fs, es) -> vs, fs, List.append (List.map (fun m -> Ev(m, f)) messages) es), f 

let rec simplify_dependent (factors : FactorId list) (G : Graph) : Graph * FactorId = 
    // when doing variable elimination, we can't just merge the 
    // neighbouring factors; need to merge all dependent factors
    // until we reach another variable that's to be eliminated
    
    let fs = List.filter (fun f -> f <> -1) factors
    if fs = [] then G, -1
    else 
        let G', f' = merge_many fs G
        let var_children = 
            children (Fid f') G'
            |> List.map (fun c -> match c with Vid x -> x) 
            |> Set.ofList
            |> fun s -> Collections.Set.difference s (Set.ofList ordering)
            |> Set.toList
            |> List.map (fun c -> Vid c) 
            
        // FIXME: we need to make sure the parents of the chldren are all 
        // merged properly, so that we don't introduce loops?

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
            let G'', f'' = simplify_dependent children G'
            merge_simple G'' f' f''


/// Eliminates variable d in graph G.
/// Returns the updated graph and the factor resulting in the elimination of d.
let eliminate (G: Graph) (d: VarId)  = 
    
    let d_arg = Factorgraph.get_variable d G

    let parents_d = parents (Vid d) G |> List.map (fun n -> match n with Fid f -> f)
    let children_d = children (Vid d) G |> List.map (fun n -> match n with Fid f -> f)

    //assert(List.allPairs parents_d children_d |> List.exists (fun (p, c) -> depends_on (Fid c) (Fid p) G) |> not)
   
    let G_messagified, messages = 
        List.fold (fun (g, ms) f -> 
                    let g', m' = messagify d_arg g f
                    g', m'::ms) (G, []) parents_d
                    
    let G', in_factor = merge_many parents_d G_messagified
    let G'', out_factor = simplify_dependent children_d G' |> elim d messages

    let G_res = G''

    (*let G_res, d_factor_res = 
        match in_factor, out_factor with 
        | -1, -1 -> failwith "unexpected"  
        | -1, _ -> G'', out_factor
        | _, -1 -> failwith "unexpected"  //simplify_dependent children_d G_end |> elim d message_name
        | _, _ -> merge_simple G'' in_factor out_factor*)
   
    
    graphviz G_res (10 - List.length ordering) (sprintf "elim_%s" d)
    ordering <- List.tail ordering
    G_res


let direct (G: Graph) : Graph =
    let V, F, E = G
    let E' = List.filter (fun e -> match e with Ef(f, v) -> List.contains (Ev(v, f)) E |> not | _ -> true) E

    V, F, E'

let eliminate_variables (graph : Graph) (order : Ide list) : S =
    // Use the ordering to eliminate the discrete variables 
    // one by one. Merge the resulting graph in a single statement.

    let silly_prog_merge ((_, F, _) : Graph) =
        SofList (List.map fst F)

    ordering <- order

    let interm = List.fold eliminate graph order
    let vs, _, _ = interm

    interm 
    |> direct 
    |> dfs |> snd 
    // FIXME: dfs currently broken! Doesn't work for disconnected graphs
    //|> silly_prog_merge
    |> DeclOfList vs
  
