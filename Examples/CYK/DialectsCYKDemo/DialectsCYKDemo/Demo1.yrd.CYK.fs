module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator
type cykToken = 
  | EOF
  | NUM
  | PLUS
  | MULT
let getTag token = 
  match token with 
  | EOF -> 0us
  | NUM -> 1us
  | PLUS -> 2us
  | MULT -> 3us
let rules = 
  [
  281483566841856UL; 562954248388608UL; 1125908496777216UL; 1407387768455424UL;
  844442110132224UL; 844446405099520UL; 844442110066688UL ]
  |> Array.ofList
let StartNTerm = 1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream
  |> Seq.choose (fun t ->
    let tag = getTag t.Tag
    if tag <> 0us then Some tag else None)
  |> Array.ofSeq