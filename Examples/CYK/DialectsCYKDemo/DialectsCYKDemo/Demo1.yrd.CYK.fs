module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator

type cykToken = 
  | NUM
  | PLUS
  | EOF
let getTag token = 
  match token with 
  | NUM -> 1
  | PLUS -> 2
  | EOF -> -1
let rules = 
  [ 281483566841856UL; 562954248388864UL; 844433520067072UL; 844437815231232UL ]
  |> Array.ofList
let StartNTerm =  1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream |> Seq.map (fun t -> getTag t.Tag)