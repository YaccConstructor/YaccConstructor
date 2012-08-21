module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator
type cykToken = 
  | EOF
  | PLUS
  | NUM
let getTag token = 
  match token with 
  | EOF -> 0us
  | PLUS -> 1us
  | NUM -> 2us
let rules = 
  [ 281483566841856UL; 844429225099264UL; 562958543355904UL; 562962838454272UL ]
  |> Array.ofList
let StartNTerm = 1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream
  |> Seq.choose (fun t ->
    let tag = getTag t.Tag
    if tag <> 0us then Some tag else None)
  |> Array.ofSeq