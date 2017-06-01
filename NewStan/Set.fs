module Set

open NewStanSyntax

type Context = (Type * Ide) Set 

let singleton (element : Ide) : Context =
    // FIXME: you might wish to make the case with
    // a single element prettier...
    Set.empty |> Set.add ((Real,Data), element)

let contains (x:string) (c:Context) : bool =
    let names = Set.map (fun (t,n) -> n) c
    Set.contains x names

let intersectNames (c1:Context) (c2:Context) : Set<Ide> =
    let c1' = Set.map (fun (t,v) -> v) c1 
    let c2' = Set.map (fun (t,v) -> v) c2 

    Set.intersect c1' c2'

let intersectEmpty (c1:Context) (c2:Context) : bool =
    Set.isEmpty (intersectNames c1 c2)

let intersect (c1:Context) (c2:Context) : Context =
    failwith "intersect not implemented for type Context"
    
    
    