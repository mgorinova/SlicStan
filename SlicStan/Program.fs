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
open Factorgraph

open Tests

open ConstraintSolver


exception TranslationException of string

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res    
    

let examples = typeof<Examples.ExamplesModule.ExamplesType>.DeclaringType.GetMethods()
            
let enum_with_exception s d = 
    //try
    Enumerate.enum s d
    //with _ -> 
    //    raise (TranslationException (sprintf "Could not eliminate discrete variable %s" d))

let run_single (slic : string) = 

    let parsed = try parse slic with _ -> raise (TranslationException "Could not parse")
    toplevel <- true
    let typechecked = //typecheck_Prog parsed
        try typecheck_Prog parsed with _ -> raise (TranslationException "Could not typecheck")
    
    let W, graph = Factorgraph.to_graph (snd typechecked)
    let ordering = Factorgraph.find_ordering W graph
    //let ordering = ["z2"; "z3"; "z4"; "z1"]
    //let ordering = ["d3"; "d2"; "d1"; "d4"; "d5"]
    //let ordering = ["sim"]
 
    let elaborated = try elaborate_Prog typechecked with _ -> raise (TranslationException "Could not elaborate")
    
    let gamma, enum = //List.fold (Enumerate.enum) elaborated ordering
        List.fold (enum_with_exception) elaborated ordering

    //printfn "\n\nSlicStan reduced:\n\n%A" (SlicStanSyntax.S_pretty "" enum)
    
    let sd, sm, sq = try shred_S gamma enum with _ -> raise (TranslationException "Could not shred final program")

    let stan = try transform gamma (sd, sm, sq) with _ ->  raise (TranslationException "Failed final translation to Stan")    
    stan



let run_many (xs : System.Reflection.MethodInfo []) =
    for x in xs do
        try
            let slic = (x.Invoke(None, [||])).ToString()
            
            try 
                let _ = run_single slic
                printfn "OK"
            with 
                TranslationException message ->  
                    let name = ((x.Name).[4..(x.Name |> String.length)-1])
                    printfn "%s failed: %s" name message

        with _ -> ()



let example = Examples.ExamplesModule.discrete_madeup

[<EntryPoint>]
let main argv =   

    run_single (Examples.ExamplesModule.discrete_ifs_sep)

    //run_many examples

    printfn "%A" (resolve([Leq(Data, LevelVar "l1"), ""; Leq(Model, Lub([LevelVar "l2"; Data])), ""], ["l1"; "l2"]))
    printfn "%A" (resolve_semilattice([Leq(Data, LevelVar "l1"), ""; Leq(Model, Lub([LevelVar "l2"; Lz])), ""], ["l1"; "l2"]))

    printfn "%A" (resolve_semilattice([
                    Leq(Lub([Lub([Data; Lz])]), LevelVar "l1"), ""; 
                    ], ["l1"]))

    let option = try argv.[0] with _ -> "--no-input"
    
    let slic = match option with 
               | "--fromfile" -> 
                    let reader = File.OpenText(argv.[1])
                    reader.ReadToEnd()

               | "--no-input" -> example

               | _ -> option

    printfn "%s\n\n" slic 

    printfn "%A" (run_single slic |> MiniStanSyntax.Prog_pretty)

    
    0

