﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SlicStanSyntax

open FSharp.Text.Lexing
open Parser
open Lexer 

open Typecheck
open Enumerate
open Elaborate
open Shredding
open Transformation

open Factorgraph
open Constraints

open System.Diagnostics;

open System.IO

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res    

let example = Examples.discrete1
 
// look into array elements dependance analysis literature
  
// enumerate only ints of level models that are not TP

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
    

    (*  On discrete parameters support
        
        The keyword `enum` from the Overleaf document, really coinsides with
        the scope of each discrete parameter (level MODEL variable).
        This is possibly similar to Pyro's `plate`.

        Our goal is essentially to figure out how to minimise and 
        decouple scopes as much as possible. 

        Then each discrete variable declaration, together with the statment 
        in its scope, is transformed to explicit ennumeration and 
        generation of discrete variables using BP-guided sampling.
    *)

    let typechecked = slic
                    |> parse
                    |> typecheck_Prog 

    let graph = Factorgraph.to_graph (snd typechecked)

    printfn "%A" (pp_graph graph)

    let enum = enumerate_Prog (snd typechecked)

    printfn "\n\n%s" (SlicStanSyntax.S_pretty "" enum)
    


    // TODO for next working day: 
    // (1) figure out a lighter form of elaboration --- data and parameters 
    //     declarations need to happen at the top level.
    // (2) think about how to implement the array discrete variables support.
    // (3) translation very broken at the moment.
    

    
    
    let ctx, s =  elaborate_Prog ([], enum)

    let sd, sm, sq = shred_S ctx s 

    let stan = transform ctx (sd, sm, sq)
    
    printfn "%s" (MiniStanSyntax.Prog_pretty stan)      


    0



