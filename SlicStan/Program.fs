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


let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res    


// d1 -> d2 -> d3 <- d4 <- d5

let example = Examples.discrete_chain
let name = Util.get_var_name <@Examples.discrete_chain@>
printfn "Name is %s" name
set_folder (name)

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

    (*  BOOKMARK: ToDo next
        * Figure out what to do in the "tree" case: possibly something like
          instead of parents and children, we look at the side that has been
          computed vs the side that hasn't. Always only one child/parent is 
          on the side that hasn't been computed. This + smarter decision re
          the order of elimination should work, I think!
        
        (check) * Figure out the "backward" pass: shouldn't be that hard when a 
          chain but what do we do when the structure is more complicated?
        
        * Generating variables might need fixing to generated transform discrete 
          vars correctly. 
        
        (!!!)* Generating in a tree seems to get messed up and/or I haven't dones
          the backward pass properly. FIXME!
        
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


    let W, graph = Factorgraph.to_graph (snd typechecked)
    graphviz graph 0 "init"
    let ordering = Factorgraph.find_ordering W graph //|> List.rev
    //let ordering = ["d3"; "d2"; "d1"; "d4"; "d5"]
    printfn "Ordering:\n%A" ordering

    let elaborated =  elaborate_Prog typechecked
    
    let gamma, enum = Enumerate.enum elaborated
    
    printfn "\n\nSlicStan reduced:\n%A" (SlicStanSyntax.S_pretty "" enum)

    //let gamma, s = elaborated

    let sd, sm, sq = shred_S gamma enum

    let stan = transform gamma (sd, sm, sq)
    
    printfn "%s" (MiniStanSyntax.Prog_pretty stan)      

    0



