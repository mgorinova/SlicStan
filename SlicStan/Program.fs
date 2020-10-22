// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SlicStanSyntax

open FSharp.Text.Lexing
open Parser
open Lexer 

open Typecheck
open Elaborate
open Enumerate
open Shredding
open Transformation

open System.Diagnostics;

open System.IO

open ConstraintSolver
open System.Collections.Generic


exception TranslationException of string * string

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res    
    

let examples = typeof<Examples.ExamplesModule.ExamplesType>.DeclaringType.GetMethods()
            
let enum_with_exception s d = 
    try Enumerate.enum s d
    with _ -> 
        raise (TranslationException (sprintf "Could not eliminate discrete variable %s" d,
                                     sprintf "\n%A\n\n%A" (fst s) (snd s |> SlicStanSyntax.S_pretty "")))

let run_single (slic : string) = 

    let parsed = try parse slic with _ -> raise (TranslationException ("Could not parse", ""))
    toplevel <- true
    let typechecked = try typecheck_Prog parsed with _ -> raise (TranslationException ("Could not typecheck", ""))

    let elaborated = 
        try elaborate_Prog typechecked 
        with _ -> raise (TranslationException ("Could not elaborate", ""))
    
    let ordering =
        try 
            let W, graph = Factorgraph.to_graph (snd typechecked)
            Factorgraph.find_ordering W graph
        with _ -> 
            // raise (TranslationException ("Could not find elimination ordering", ""))
            let W = Util.assigns_global (snd elaborated)
            Map.filter (fun name (tp, tl) -> tl = Model && tp <. Int && (Set.contains name W |> not)) (fst elaborated)
            |> Map.toList
            |> List.map fst

    let gamma, enum = List.fold (enum_with_exception) elaborated ordering
    
    let sd, sm, sq = 
        try shred_S gamma enum 
        with _ -> 
            raise (TranslationException ("Could not shred final program", SlicStanSyntax.S_pretty "" enum))

    //printfn "SD: \n%s\n\nSM: \n%s\n\nSQ: \n%s\n" ( SlicStanSyntax.S_pretty "" sd ) ( SlicStanSyntax.S_pretty "" sm ) ( SlicStanSyntax.S_pretty "" sq )

    let stan = 
        try transform gamma (sd, sm, sq) 
        with _ -> raise ( TranslationException (
                            "Failed final translation to Stan",
                            let sd', sm', sq' = SlicStanSyntax.S_pretty "" sd, SlicStanSyntax.S_pretty "" sm, SlicStanSyntax.S_pretty "" sq 
                            sprintf "\n//Data:\n%A\n//Model:\n%A'\n//GenQuant:\n%A" sd' sm' sq')
                        )
    stan
    


let run_many (xs : System.Reflection.MethodInfo [], writeOutput) =

    if writeOutput then Directory.CreateDirectory("/transformed_examples/") |> ignore

    for x in Array.filter (fun (m: System.Reflection.MethodInfo) -> m.IsStatic) xs do
        let slic = (x.Invoke(None, [||])).ToString()
        let name = ((x.Name).[4..(x.Name |> String.length)-1])
           
        try 
            let transformed = run_single slic |> MiniStanSyntax.Prog_pretty           
            printfn "OK"
            if writeOutput 
            then File.WriteAllText(sprintf "/transformed_examples/%s.stan" name, transformed)

        with TranslationException (message, lastProg) ->  
            printfn "%s: %s." message name
            if writeOutput 
            then File.WriteAllText(sprintf "/transformed_examples/%s.slic" name, sprintf "//%s\n%s" message (lastProg))
            

[<EntryPoint>]
let main argv =

    let option = try argv.[0] with _ -> "--all"
    
    //let option = "--from-name"
    let name = "discrete_repo"

    match option with 
    | "--test-all" -> run_many (examples, false)
    | "--all" -> run_many (examples, true)
    | "--from-file" -> 
        let slic = File.OpenText(argv.[1]).ReadToEnd()
        printfn "%s" (run_single slic |> MiniStanSyntax.Prog_pretty)
    | "--from-name" -> 
        let method = (Array.filter (fun (m: System.Reflection.MethodInfo) -> m.Name.Contains (name)) examples).[0]
        let slic = (method.Invoke(None, [||])).ToString()
        printfn "%s" (run_single slic |> MiniStanSyntax.Prog_pretty)

    | _ -> 
        let slic = option
        printfn "%s" (run_single slic |> MiniStanSyntax.Prog_pretty)
    
    0

