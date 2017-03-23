module NewStanSyntax

type Ide = string

type Exp = Var of Ide
         | Const of double
         | Plus of Exp * Exp
         | Mul of Exp * Exp
         | Prim of string * List<Exp>

type Dist = Dist of string * List<Exp>

type S = Sample of Ide * Dist
       | Data of Ide
       | Let of Ide * Exp
       | Seq of S * S
       | Skip
       | PrimDef of Ide * List<Ide> * S // a function could return an Exp, a Dist or nothing
       | Call of Ide * List<Exp>


let rec E_pretty E =
  match E with
  | Var(x) -> x
  | Const(d) -> sprintf "%O" d
  | Plus(e1, e2) -> E_pretty e1 + " + " + E_pretty e2
  | Mul(e1, e2) -> E_pretty e1 + " * " + E_pretty e2
  | Prim(p,[]) -> sprintf "%s()" p
  | Prim(p,Es) -> sprintf "%s(%s)" p (List.reduce (fun s1 s2 -> s1+","+s2) (List.map E_pretty Es))

and D_pretty D =
  match D with
  | Dist(p,[]) -> sprintf "%s()" p
  | Dist(p,Es) -> sprintf "%s(%s)" p (List.reduce (fun s1 s2 -> s1+","+s2) (List.map E_pretty Es))

let rec S_pretty S =
  match S with
  | Data(x) -> sprintf "data %s;" x
  | Sample(x,D) -> sprintf "%s ~ %s;" x (D_pretty D)
  | Let(x,E) -> sprintf "%s = %s;" x (E_pretty E)
  | Seq(S1,S2) -> sprintf "%s \n%s" (S_pretty S1) (S_pretty S2)
  | Skip -> ""
  | PrimDef(f,[],S) -> sprintf "def %s() { %s }" f (S_pretty S)
  | PrimDef(f,ys,S) -> sprintf "def %s(%s) { %s }" f (List.reduce (fun s1 s2 -> s1+","+s2) ys) (S_pretty S)
  | Call(x,[]) -> sprintf "%s()" x 
  | Call(x,Es) -> sprintf "%s(%s)" x (List.reduce (fun s1 s2 -> s1+","+s2) (List.map E_pretty Es))

// helper function to build a statement from a statement list
let rec S_of_list Ss =
  match Ss with
  | [] -> Skip
  | [S] -> S
  | S::Ss' -> Seq(S,S_of_list Ss')



