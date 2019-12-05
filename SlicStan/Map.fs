module Map

open SlicStanSyntax

type Gamma = Map <Ide, Type> 

let singleton (element : Ide) : Gamma =
    // FIXME: you might wish to make the case with
    // a single element prettier...
    Map.empty |> Map.add element (Real, Data)

let gammaContains (x: string) (g: Gamma) : bool =
    Map.containsKey x g

let gammaItemTypePrim (x: string) (g: Gamma) : TypePrim =
    if x = "target" then Real
    else Map.find x g |> fst     

let gammaItemTypeLevel (x:string) (g: Gamma) : TypeLevel =
    if x = "target" then Model
    else Map.find x g |> snd

let intersectNames (g1: Gamma) (g2: Gamma) : Set<Ide> =
    let g1' = Map.toList g1 |> List.map (fun (k, v) -> k) |> Set.ofList
    let g2' = Map.toList g2 |> List.map (fun (k, v) -> k) |> Set.ofList

    Set.intersect g1' g2'

let intersectEmpty (g1: Gamma) (g2: Gamma) : bool =
    Set.isEmpty (intersectNames g1 g2)

let intersect (g1: Gamma) (g2: Gamma) : Gamma =
    failwith "intersect not implemented for type Gamma"

let union (g1: Gamma) (g2: Gamma) : Gamma =
    Map(Seq.concat [ (Map.toSeq g1) ; (Map.toSeq g2) ])
