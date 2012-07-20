module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator
type cykToken = 
  | EOF
  | NUM
  | PLUS
let getTag token = 
  match token with 
  | EOF -> 0us
  | NUM -> 1us
  | PLUS -> 2us
let rules = 
  [ 281483566841856UL; 562954248388864UL; 844433520067072UL; 844437815231232UL ]
  |> Array.ofList
let StartNTerm = 1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream
  |> Seq.choose (fun t ->
    let tag = getTag t.Tag
    if tag <> 0us then Some tag else None)
  |> Array.ofSeq