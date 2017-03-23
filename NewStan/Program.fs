// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open NewStanSyntax
open Types
open Examples
//open MiniStanSyntax
open Translate
open Checker

[<EntryPoint>]
let main argv = 

    let ex = Examples.ex2
    //let example_stan = MiniStanSyntax.example
    
    printfn "Program: %A\n" (NewStanSyntax.S_pretty ex)

    let env = Types.check_S ex Map.empty
    printfn "Levels: %A\n" (env)

    let data_and_model = Types.check_data_and_model ex
    printfn "Data & Params: %A\n" (data_and_model)

    let ex_stan = Translate.translate ex env data_and_model 

    printfn "Stan: \n%s" (MiniStanSyntax.Prog_pretty ex_stan)

    let ex = S_of_list [Data("y1");
                        NewStanSyntax.Let("alpha",Const(0.1));
                        NewStanSyntax.Let("beta",Const(0.1));
                        NewStanSyntax.Sample("tau_y",Dist("gamma",[Var"alpha";Var"beta"]));
                        NewStanSyntax.Let("sigma_y",Prim("pow",[Var"tau_y";Const(-0.5)]));
                        NewStanSyntax.Sample("mu_y",Dist("normal",[Const(0.0);Const(1.0)]));
                        NewStanSyntax.Sample("y1",Dist("normal",[Var"mu_y";Var"sigma_y"]));
                        NewStanSyntax.Let("variance_y",Prim("pow",[Var"sigma_y";Const(2.0)]))]

    let gamma =[("y1", ModelLevel); ("alpha", DataLevel); ("beta", DataLevel); 
                ("tau_y", ModelLevel); ("sigma_y", ModelLevel); ("mu_y", ModelLevel);
                ("variance_y", GenQuantLevel)]

    printfn "Typecheck: %A" (Checker.type_check_S ex ModelLevel gamma)

    0



