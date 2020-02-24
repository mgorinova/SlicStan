module Util

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open SlicStanSyntax


let get_var_name (e:Expr) =
  match e with
    | PropertyGet (e, pi, li) -> pi.Name
    | _ -> failwith "not a let-bound value"


let flip_tuple (a,b) = (b,a)

// helper function to build a statement from a statement list
let rec SofList ss =
  match ss with
  | [] -> Skip
  | [s] -> s
  | s::ss' -> Seq(s, SofList ss')


let rec DeclOfList env s = 
    match env with 
    | [] -> s
    | x::xs -> Decl(x, DeclOfList xs s)


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

let to_lpf (d:Dist) (e:Exp) =
    // FIXME: only works for discrete distributions; need to extend
    match d with
    | Dist(name, args) -> Prim(name + "_lpmf", e::args )
    | _ -> failwith "user defined distributions not supported"

let rec indices_list_to_lhs lhs_base exp_list =
    match exp_list with
    | [] -> lhs_base
    | i::list -> indices_list_to_lhs (A(lhs_base, i)) list

let rec indices_list_to_exp exp_base exp_list =
    match exp_list with
    | [] -> exp_base
    | i::list -> indices_list_to_exp (ArrElExp(exp_base, i)) list

let rec assigns (S: S) : Set<Ide> =
    match S with
    | Decl((_, x), s) -> Set.remove x (assigns s)
    | Sample(x, _) -> Set.empty
    | Factor _ -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns s1) (assigns s2)
    | For(x, l, u, s) -> assigns s
    | Seq(s1, s2) -> Set.union (assigns s1) (assigns s2)
    | Skip -> Set.empty
    | Message(arg, args, s) -> Set.remove (snd arg) (assigns s)
    | Elim(arg, s) -> Set.remove (snd arg) (assigns s)
    | Generate(arg, s) -> assigns s


let rec assigns_global (S: S) : Set<Ide> = 
    // FIXME: should we use the usual assigns from above? 
    // Should we deal with elaborated statements only?
    // Should elaboration be as is, or should we think more about locally defined parameters? 
    match S with
    | Decl((_, x), s) -> assigns_global s
    | Sample(x, _) -> Set.empty
    | Factor _ -> Set.empty
    | Assign(lhs, _) -> Set.add (LValueBaseName lhs) (Set.empty)
    | If(e, s1, s2) -> Set.union (assigns_global s1) (assigns_global s2)
    | For(x, l, u, s) -> assigns_global s
    | Seq(s1, s2) -> Set.union (assigns_global s1) (assigns_global s2)
    | Skip -> Set.empty
    | Message(arg, _, s) -> Set.add (snd arg) (assigns_global s)
    | Elim(_, s) -> assigns_global s
    | Generate(v, s) -> Set.add (snd v) (assigns_global s)


let rec read_exp (E: Exp) : Set<Ide> =
    match E with 
    | Var(x) -> Set.add x Set.empty
    | Const _ -> Set.empty
    | Arr(list) -> Set.unionMany (List.map read_exp list |> List.toSeq)
    | ArrElExp(e1, e2) -> Set.union (read_exp e1) (read_exp e2)
    | Prim(name, list) -> Set.unionMany (List.map read_exp list |> List.toSeq)
    | ECall _ -> failwith "not impl"
    | Plus(e1, e2) -> Set.union (read_exp e1) (read_exp e2)
    | Mul(e1, e2) -> Set.union (read_exp e1) (read_exp e2)


let read_dist (D: Dist) : Set<Ide> =
    match D with 
    | Dist(name, list) -> Set.unionMany (List.map read_exp list |> List.toSeq)

let read_size (a : ArrSize) : Ide list = 
    match a with 
    | N _ -> []
    | SizeVar x -> [x]

let rec reads (S: S) : Set<Ide> =
    match S with
    | Decl(_, s) -> reads s
    | Sample(lhs, d) -> Set.union (lhs_to_exp lhs |> read_exp) (read_dist d) // FIXME: changed this from something that was ignoring e: not sure what that changes?
    | Factor(e) -> read_exp e
    | Assign(lhs, e) -> 
        Set.union (lhs_to_exp lhs |> read_exp) (read_exp e)
        //|> fun set -> 
        //    if Set.contains (LValueBaseName lhs) set 
        //    then Set.union (lhs_to_exp lhs |> read_exp) set 
        //    else Set.union (lhs_to_exp lhs |> read_exp) set |> Set.remove (LValueBaseName lhs)
    | If(e, s1, s2) -> Set.union (reads s1) (reads s2) |> Set.union (read_exp e)
    | For(i, l, u, s) -> 
        [ ( match l with N _ -> "" | SizeVar x -> x );
          ( match u with N _ -> "" | SizeVar x -> x ) ]
        |> List.filter (fun x -> x <> "")
        |> Set.ofList
        |> Set.union (reads s)
        |> Set.filter (fun x -> x <> snd i)
    | Seq(s1, s2) -> Set.union (reads s1) (reads s2)
    | Skip -> Set.empty
    | Message(arg, args, s) -> Set.difference (reads s) (Set.ofList args) 
    | Elim(arg, s) -> Set.remove (snd arg) (reads s) 
    | Generate(arg, s) -> reads s







