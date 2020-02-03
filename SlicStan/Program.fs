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

open Constraints

open System.Diagnostics;

open System.IO
open Factorgraph

open Tests


let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res    

// d1 -> d2 -> d3 <- d4 <- d5

let example = Examples.sprinkler_ifs
let name = Util.get_var_name <@Examples.sprinkler_ifs@>
printfn "Name is %s" name
set_folder (name)

// enumerate only ints of level model that are not TP

[<EntryPoint>]
let main argv =   

    let option = try argv.[0] with _ -> "--no-input"
    
    let slic = match option with 
               | "--fromfile" -> 
                    let reader = File.OpenText(argv.[1])
                    reader.ReadToEnd()

               | "--no-input" -> example

               | _ -> option

    printfn "%s\n\n" slic 
    
    (*  BOOKMARK: ToDo next
        * Make transformed variables truley local to avoid mistakes. 
        * Think of a way to generalise the whole "what sort of shredding are 
          we doing", with a "criteria" or something like that. 
        ----------------------------------------------------------------------
        * Think about discrete variable arrays. It will be a shame if we don't 
          implemnt this. Yes, we can unroll the loops for loop bounds known 
          at static time, but really I think we can do better. 

        * A natural step is to implemented the junction tree algorithm, so 
          that loops between discrete variables can also be implemented. But
          is more of an extra; shouldn't be needed for a paper?    
    *)

    let typechecked = slic
                    |> parse
                    |> typecheck_Prog 


    // Tests.test_neighbour()

    let W, graph = Factorgraph.to_graph (snd typechecked)
    graphviz graph 0 "init"
    let ordering = Factorgraph.find_ordering W graph // |> List.rev
    //let ordering = ["z1"; "z2"]
    //let ordering = ["d3"; "d2"; "d1"; "d4"; "d5"]

    printfn "Elimination ordering:\n%A" ordering

    let elaborated =  elaborate_Prog typechecked
    
    let gamma, enum = 
        List.fold (Enumerate.enum) elaborated ordering
    
    printfn "\n\nSlicStan reduced:\n\n%A" (SlicStanSyntax.S_pretty "" enum)
    

    let sd, sm, sq = shred_S gamma enum

    let stan = transform gamma (sd, sm, sq)
    
    printfn "\n\nStan translation:\n\n%s" (MiniStanSyntax.Prog_pretty stan)      

    0

