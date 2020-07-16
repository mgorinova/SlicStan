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
    