// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open NewStanSyntax
open Examples
open Elaborate
open Translate


[<EntryPoint>]
let main argv = 

    //let ex = Examples.ex_simple_normal
    let ex = Examples.ex_mynormal
    //let ex = Examples.ex_linear_funcs
    //let ex = Examples.ex_mynormal_clash
    //let ex = Examples.ex_mynormal_clash2
    //let ex = Examples.ex_mynormal_clash3
    //let ex = Examples.ex_arrays
    //let ex = Examples.ex_multinormal

    let gamma = Map.empty
    let defs, prog = ex


    printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)

   
    //let args, cdefs, sdefs, _ = Elaborate.elaborate_F defs (List.head defs)
    //printfn "Arguments: %A" (args)
    //printfn "Context: %A" (cdefs)
    //printfn "Function: %s" (NewStanSyntax.S_pretty "" sdefs)

    printfn "%A" (Typecheck.typecheck_Prog ex)

    
    let context, elab = Elaborate.elaborate_NewStanProg ex

    printfn "Context: %A" (context)

    printfn "Elaborated:\n%s" (NewStanSyntax.S_pretty "" elab)

    let data, model = Elaborate.check_data_and_model elab

    printfn "\ndata: %A\nmodel: %A\n" data model

    printfn "Translated:\n%s" (MiniStanSyntax.Prog_pretty (Translate.translate context elab (data, model)))
    0



