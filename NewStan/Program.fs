// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open NewStanSyntax

open FSharp.Text.Lexing
open Parser
open Lexer 

open Examples
open Typecheck
open Enumerate
open Elaborate
open Translate

open Constraints

open System.Diagnostics;
open System.IO

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res    

let example = Examples.soft_k_means
 
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
                    
    //let ctx, elab =  elaborate_Prog typechecked

    //printf "\n\n%s" (NewStanSyntax.S_pretty "" elab)

    let enum = enumerate_Prog (snd typechecked)

    printf "\n\n%s" (NewStanSyntax.S_pretty "" enum)
    
    // TODO for next working day: 
    // (1) fix parser
    // (2) think about how to implement the array discrete variables support
    

    // ide is "identifier", it's a string.
    // gamma, ctx, context, etc are \Gamma from the latex typing rules.
    (*
    let ctx, s = elab

    printf "%A\n\n%A\n\n" ctx (NewStanSyntax.S_pretty "" s)

    let stan = translate ctx s
    
    // Not in this version:
    // string -> parse -> elborate -> typecheck -> shred -> translate

    printfn "%s" (MiniStanSyntax.Prog_pretty stan)      *)


    0



