module Types

open NewStanSyntax

type TypeLevel = DataLevel | ModelLevel | GenQuantLevel | OptionLevel of TypeLevel list

let (<=) l1 l2 =
    match l1 with
    | DataLevel -> true
    | ModelLevel ->
        match l2 with 
        | DataLevel -> false
        | _ -> true
    | GenQuantLevel ->
        match l2 with
        | GenQuantLevel -> true
        | OptionLevel (tl) ->  List.exists ((=) GenQuantLevel) (tl)
        | _ -> false
    | OptionLevel (tl1) -> 
        match l2 with
        | OptionLevel (tl2) ->  List.fold (fun s b -> s && b) true [for el in tl1 do yield (List.exists ((=) el) (tl2))] 
        | _ -> false


type TypePrim = string
type Type = TypePrim * TypeLevel
type Dict = Microsoft.FSharp.Collections.Map<Ide,Type>


let rec min_Level (list : TypeLevel list) : TypeLevel =
    match list with
    | x::xs -> if x <= min_Level xs then x else min_Level xs
    | []    -> GenQuantLevel

let rec max_Level (list : TypeLevel list) : TypeLevel =
    match list with
    | x::xs -> if x <= max_Level xs then max_Level xs else x
    | []    -> DataLevel


let to_option level =

    let lev = match level with OptionLevel (list) -> max_Level list | l -> l

    let ret = List.filter (fun l -> lev <= l) [DataLevel; ModelLevel; GenQuantLevel]
    if ret.Length = 1 
    then match ret with 
         | x::[] -> x
         | _ -> failwith "unexpected"
    else OptionLevel(ret)


let join (ds:Dict list) = 
    Map(Seq.concat (List.map (fun d -> Map.toSeq d) ds))


// check that the expression e is well-formed and return its type
let rec check_Exp (e: Exp) (env: Dict) : Dict = 

    match e with
    | Var(x) -> if env.ContainsKey(x) 
                then Map([x, env.Item(x)]) 
                else failwith "Unexpected! Key not present in environment."
    | Const(d) -> Map.empty.Add("", ("real", DataLevel))
    | Plus(e1,e2) -> join [(check_Exp e1 env); (check_Exp e2 env)]
    | Mul (e1,e2) -> join [(check_Exp e1 env); (check_Exp e2 env)]
    | Prim(p,[]) -> Map.empty.Add("", ("real", DataLevel))
    | Prim(p,Es) -> 
        let typeList = List.map (fun ee -> check_Exp ee env) Es 
                    |> join
        typeList


let rec check_Dist (d: Dist) (env: Dict) : Dict = 
    match d with
    | Dist(_, Es) -> 
        let typeList = List.map (fun ee -> check_Exp ee env) Es 
                    |> join
        typeList

// x <- E  then level(E) <= level(x)
// x ~  D  then level(x) = model and level(D) <= model

/// Check that a statement S is well-formed, and return a dictionary of the defined identifiers
/// and their type (primitive type + level).
let rec check_S (S: S) (env: Dict) : Dict = 
    failwith "not implemented"
  (*match S with
  (*| Data(x) -> env.Add(x, ("real", DataLevel))*)
  | Sample(x,D) -> 
       // let newenv = constrain env (check_Dist D env) ModelLevel
       // newenv.Add(x, ("real", ModelLevel))

  (*| Assign(x,E) -> env.Add(x, ("real", to_option (max_Level (List.map (fun (_,(_,v)) -> v) (Map.toList (check_Exp E env))))))*)
  
  | Seq(S1,S2) -> 
        let newenv = check_S S1 env
        check_S S2 newenv

  | Skip -> env
  | VCall(x,[]) -> failwith "Call not implemented"
  | VCall(x,Es) -> failwith "Call not implemented"*)



/// Check that a statement S is well-formed, and return a tuple of lists:
/// the first list contains the names of the data variables, while the 
/// second --- the names of the modeled variables (defined through ~)
let rec check_data_and_model (S: S) : string list * string list = 
  match S with
  (*| Data(x) -> ([x], [])*)
  | Sample(x,D) -> ([], [x])
  | Assign(x,E) -> ([], [])  
  | Seq(S1,S2) -> 
        let s1d, s1p = check_data_and_model S1
        let s2d, s2p = check_data_and_model S2
        (List.append s1d s2d, List.append s1p s2p)

  | Skip -> ([], [])
  | VCall(x,[]) -> failwith "Call not implemented"
  | VCall(x,Es) -> failwith "Call not implemented"