module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator
type cykToken = 
  | EOF
  | NUM
  | BAR
  | STAR
  | PLUS
  | MULT
let getTag token = 
  match token with 
  | EOF -> 0us
  | NUM -> 1us
  | BAR -> 2us
  | STAR -> 3us
  | PLUS -> 4us
  | MULT -> 5us
let rules = 
  [
  281483566841856UL; 281479271677952UL; 1125908496777472UL; 1125912791745024UL;
  1407392063422464UL; 1407396358389760UL; 1407383473488128UL; 1407387768455680UL;
  844446405033984UL; 562954248388608UL ]
  |> Array.ofList
let StartNTerm = 1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream
  |> Seq.choose (fun t ->
    let tag = getTag t.Tag
    if tag <> 0us then Some tag else None)
  |> Array.ofSeq