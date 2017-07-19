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

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res


[<EntryPoint>]
let main argv =   
    
    let ex = parse Examples.mynormal_clash3
    printfn "%A\n\n" ex 

    let gamma = Map.empty
    let defs, prog = ex

    printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)
    //
    let inferred = typecheck_Prog ex 
        
    let context, elab = Elaborate.elaborate_NewStanProg inferred

    printfn "Context: %A" (context)

    printfn "Elaborated:\n%s" (NewStanSyntax.S_pretty "" elab)

    printfn "Translated:\n%s" (MiniStanSyntax.Prog_pretty (Translate.translate context elab))
    
    
    0



