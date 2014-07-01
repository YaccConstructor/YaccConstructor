module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator
type cykToken = 
  | EOF
  | NUM
let getTag token = 
  match token with 
  | EOF -> 0us
  | NUM -> 1us
let rules = 
  [| 281479271677952UL |]
let lblName = 
  [|
  |]
let StartNTerm = 1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream
  |> Seq.choose (fun t ->
    let tag = getTag t.Tag
    if tag <> 0us then Some tag else None)
  |> Array.ofSeq