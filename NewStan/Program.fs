// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open NewStanSyntax
open Types
open Examples
//open MiniStanSyntax
open Elaborate
open Translate
open Checker

[<EntryPoint>]
let main argv = 

    
    (*
    //let example_stan = MiniStanSyntax.example
    
    printfn "Program: %A\n" (NewStanSyntax.S_pretty ex)

    let env = Types.check_S ex Map.empty
    printfn "Levels: %A\n" (env)

    let data_and_model = Types.check_data_and_model ex
    printfn "Data & Params: %A\n" (data_and_model)

    let ex_stan = Translate.translate ex env data_and_model 

    printfn "Stan: \n%s" (MiniStanSyntax.Prog_pretty ex_stan)
    *)

    //let ex = Examples.ex_mynormal
    //let ex = Examples.ex_linear_funcs
    let ex = Examples.ex_mynormal_clash
    //let ex = Examples.ex_mynormal_clash2

    printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)

    let elab = Elaborate.elaborate_NewStanProg ex

    printfn "Elaborated:\n%s" (NewStanSyntax.S_pretty "" elab)

    let data, model = Elaborate.check_data_and_model elab

    printfn "\ndata: %A\nmodel: %A\n" data model

    printfn "Translated:\n%s" (MiniStanSyntax.Prog_pretty (Translate.translate elab (data, model)))
    0



