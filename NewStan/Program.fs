// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open NewStanSyntax

open Microsoft.FSharp.Text.Lexing
open Parser
open Lexer 

open Examples
open Typecheck
open Elaborate
open Translate

open Constraints

open System.Diagnostics;
open System.IO

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res


(*let before = 
    let mynormal = parse Examples.mynormal 
    let fs = fst mynormal
    //let ex = parse Examples.mynormal 
     
    let s = Generation.generate_manyS 30
    let ex = fs, s
    //printfn "%A\n\n" ex 

    let gamma = Map.empty
    let defs, prog = ex
    
    printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)
    
    
    let stopwatch = new Stopwatch()
    
    stopwatch.Start()
    let inferred = typecheck_Prog ex 
    let inference_time = stopwatch.ElapsedMilliseconds;
    //printfn "%s" (NewStanSyntax.NewStanProg_pretty inferred)

    stopwatch.Restart()
    let context, elab = Elaborate.elaborate_NewStanProg inferred
    let elaboration_time = stopwatch.ElapsedMilliseconds;
    //printfn "Context: %A" (context)
    //printfn "Elaborated:\n%s" (NewStanSyntax.S_pretty "" elab)

    stopwatch.Restart()
    let translated = (Translate.translate context elab)
    let translation_time = stopwatch.ElapsedMilliseconds;
    printfn "Translated:\n%s" (MiniStanSyntax.Prog_pretty translated)
    
    printfn "Typechecked in: %d miliseconds" inference_time
    printfn "Elaborated  in: %d miliseconds" elaboration_time
    printfn "Translated  in: %d miliseconds\n" translation_time

    printfn "Original Program Stats:\n%A" (Generation.stats s)
    printfn "Stan Program Stats:\n%A" (Generation.stan_stats translated)*)
  
(*Generation 
    let size = int argv.[0]
    
    let mynormal = parse Examples.mynormal 
    let fs = fst mynormal

    //printfn "Running test..."
    let stats_some = Experiment.multiple_runs fs size 1

    //printfn "Finished. Writing to file..."
    let result = sprintf "%A" stats_some
    let name = sprintf "results/%d lines at %d-%d-%d-%d.txt" size (System.DateTime.Now.Hour) (System.DateTime.Now.Minute) (System.DateTime.Now.Second) (System.DateTime.Now.Millisecond)

    File.WriteAllText(name, result)
*)  
    

let example = Examples.ifs

[<EntryPoint>]
let main argv =   

    let option = try argv.[0] with _ -> "--no-input"
    
    let slic = match option with
               | "--fromfile" -> 
                    let reader = File.OpenText(argv.[1])
                    reader.ReadToEnd()

               | "--no-input" -> example

               | _ -> option

    // printfn "%s" slic 

    let elab = slic
            |> parse
            |> typecheck_Prog 
            |> elaborate_NewStanProg


    let ctx, s = elab

    let stan = translate ctx s
    
    printfn "%s" (MiniStanSyntax.Prog_pretty stan)      

    0



