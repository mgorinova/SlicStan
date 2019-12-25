module Tests

open Enumerate
open Examples

open Typecheck
open Elaborate

open SlicStanSyntax

open FSharp.Text.Lexing
open Parser
open Lexer 

let parse slicstan = 
    let lexbuf = LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res   

// Test Enumerate.neighbour
let test_neighbour () = 
    let discrete_chain_s = parse Examples.discrete_chain |> typecheck_Prog  |> elaborate_Prog |> snd
    if neighbour "d1" "d2" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain"
    if neighbour "d2" "d3" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain"
    if neighbour "d1" "d3" discrete_chain_s |> not then printfn "OK" else printfn "test_neighbour failed for discrete_chain"
    if neighbour "d2" "d1" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain"
    if neighbour "d3" "d2" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain"
    if neighbour "d3" "d1" discrete_chain_s |> not then printfn "OK" else printfn "test_neighbour failed for discrete_chain"

    let discrete_chain_s = parse Examples.discrete_chain_with_tp |> typecheck_Prog  |> elaborate_Prog |> snd
    if neighbour "d1" "d2" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain_with_tp"
    if neighbour "d2" "d3" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain_with_tp"
    if neighbour "d1" "d3" discrete_chain_s |> not then printfn "OK" else printfn "test_neighbour failed for for discrete_chain_with_tp"
    if neighbour "d2" "d1" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain_with_tp"
    if neighbour "d3" "d2" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_chain_with_tp"
    if neighbour "d3" "d1" discrete_chain_s |> not then printfn "OK" else printfn "test_neighbour failed for for discrete_chain_with_tp"

    let discrete_chain_s = parse Examples.discrete_hmm |> typecheck_Prog  |> elaborate_Prog |> snd
    if neighbour "z1" "z2" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_hmm"
    if neighbour "z2" "z3" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_hmm"
    if neighbour "z1" "z3" discrete_chain_s |> not then printfn "OK" else printfn "test_neighbour failed for discrete_hmm"
    if neighbour "z2" "z1" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_hmm"
    if neighbour "z3" "z2" discrete_chain_s then printfn "OK" else printfn "test_neighbour failed for discrete_hmm"
    if neighbour "z3" "z1" discrete_chain_s |> not then printfn "OK" else printfn "test_neighbour failed for discrete_hmm"