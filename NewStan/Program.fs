// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open NewStanSyntax
open Examples
open Typecheck
open Elaborate
open Translate

open Constraints

[<EntryPoint>]
let main argv = 

    //let ex = Examples.ex_simple
    //let ex = Examples.ex_simple_normal
    //let ex = Examples.ex_mynormal
    let ex = Examples.ex_neals_funnel
    //let ex = Examples.ex_neals_funnel_data
    //let ex = Examples.ex_linear_funcs 
    //let ex = Examples.ex_mynormal_clash
    //let ex = Examples.ex_mynormal_clash2
    //let ex = Examples.ex_mynormal_clash3
    //let ex = Examples.ex_arrays
    //let ex = Examples.ex_multinormal

    let gamma = Map.empty
    let defs, prog = ex


    printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)

    let inferred = typecheck_Prog ex
        
    let context, elab = Elaborate.elaborate_NewStanProg inferred

    printfn "Context: %A" (context)

    printfn "Elaborated:\n%s" (NewStanSyntax.S_pretty "" elab)

    printfn "Translated:\n%s" (MiniStanSyntax.Prog_pretty (Translate.translate context elab))
    
    
    0



