// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SlicStanSyntax

open FSharp.Text.Lexing
open Parser
open Lexer 

open Typecheck
open Elaborate
open Shredding
open Transformation

open System.IO

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res
   
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
            |> elaborate_SlicStanProg

    // ide is "identifier", it's a string.
    // gamma, ctx, context, etc are \Gamma from the latex typing rules.

    let ctx, s = elab
    let sd, sm, sq = shred_S ctx s 

    let stan = transform ctx (sd, sm, sq)
    
    // Not in this version:
    // string -> parse -> elborate -> typecheck -> shred -> translate

    printfn "%s" (MiniStanSyntax.Prog_pretty stan)      

    0



