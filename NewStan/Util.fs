module Util

open NewStanSyntax

let flip_tuple (a,b) = (b,a)

// helper function to build a statement from a statement list
let rec SofList ss =
  match ss with
  | [] -> Skip
  | [s] -> s
  | s::ss' -> Seq(s, SofList ss')


let rec BlockOfList (env, s) = 
    match env with 
    | [] -> s
    | x::xs -> Block(x, BlockOfList (xs, s))


let rec LValueBaseName (lhs: LValue): Ide =    
    match lhs with
    | I(name) -> name
    | A(lhs', _) -> LValueBaseName lhs'


let BaseTypeLevel (tau: TypeLevel) =
    match tau with
    | Data -> true
    | Model -> true
    | GenQuant -> true
    | _ -> false


let rec lhs_to_exp (lhs: LValue) =
    match lhs with
    | I(x) -> Var(x)
    | A(lhs', e) -> ArrElExp(lhs_to_exp lhs', e) 


let rec exp_to_lhs (exp: Exp) =
    match exp with
    | Var(x) -> I(x)
    | ArrElExp(lhs', e) -> A(exp_to_lhs lhs', e) 
    | _ -> failwith "something has gone terribly wrong"

let rec get_arr_el_indices acc lhs =
    match lhs with
    | I(x) -> List.rev acc
    | A(lhs', e) -> get_arr_el_indices (e :: acc) lhs' 

let get_indices lhs = 
    get_arr_el_indices [] lhs

let to_lpf (d:Dist) (x:LValue) =
    // FIXME: only works for discrete distributions; need to extend
    match d with
    | Dist(name, args) -> Prim(name + "_lpmf", (lhs_to_exp x)::args )
    | _ -> failwith "user defined distributions not supported"

let rec indices_list_to_lhs lhs_base exp_list =
    match exp_list with
    | [] -> lhs_base
    | i::list -> indices_list_to_lhs (A(lhs_base, i)) list


let rec assigns (S: S) : Set<Ide> =
    match S with
    | DataDecl(_, x, s) -> assigns s
    | Block((_, x), s) -> Set.remove x (assigns s)
    | Sample(x, _) -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns s1) (assigns s2)
    | For(x, l, u, s) -> assigns s
    | Seq(s1, s2) -> Set.union (assigns s1) (assigns s2)
    | Skip -> Set.empty
    | VCall _ -> Set.empty // FIXME: it should probably deal with the arguments? Or should it? 


let rec assigns_global (S: S) : Set<Ide> = 
    // FIXME: should we use the usual assigns from above? 
    // Should we deal with elaborated statements only?
    // Should elaboration be as is, or should we think more about locally defined parameters? 
    match S with
    | DataDecl(_, x, s) -> assigns_global s
    | Block((_, x), s) -> assigns_global s
    | Sample(x, _) -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns_global s1) (assigns_global s2)
    | For(x, l, u, s) -> assigns_global s
    | Seq(s1, s2) -> Set.union (assigns_global s1) (assigns_global s2)
    | Skip -> Set.empty
    | VCall _ -> failwith "not implemented"



