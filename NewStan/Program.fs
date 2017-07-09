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

[<EntryPoint>]
let main argv =   
    
    let parse slicstan = 
        let lexbuf = LexBuffer<char>.FromString slicstan
        let res = Parser.start Lexer.read lexbuf
        res


    let full_example = "
    def mynormal(real m, real s)
    {
        real xr;
        xr ~ normal(0, 1);
        return m + xr*s;
    }
    data real y;
    real a;
    a = mynormal(y, 0.5);"

    let ex = parse Examples.mymultinormal
    printfn "%A\n\n" ex 

    (*let examples = ["Simple", Examples.ex_simple;
                    "Simple Normal", Examples.ex_simple_normal;
                    "My Normal", Examples.ex_mynormal;
                    "Neals Funnel", Examples.ex_neals_funnel;
                    "Neals Funnel with Data", Examples.ex_neals_funnel_data;
                    "Linear Regression", Examples.ex_linear_funcs; 
                    "Naming Clash 1", Examples.ex_mynormal_clash;
                    "Naming Clash 2", Examples.ex_mynormal_clash2;
                    "Naming Clash 3", Examples.ex_mynormal_clash3;
                    "Arrays Simple", Examples.ex_arrays;
                    "My Multinormal", Examples.ex_multinormal;]*)

    let gamma = Map.empty
    let defs, prog = ex

    printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)

    let inferred = typecheck_Prog ex
        
    let context, elab = Elaborate.elaborate_NewStanProg inferred

    printfn "Context: %A" (context)

    printfn "Elaborated:\n%s" (NewStanSyntax.S_pretty "" elab)

    printfn "Translated:\n%s" (MiniStanSyntax.Prog_pretty (Translate.translate context elab))
    
    
    0



