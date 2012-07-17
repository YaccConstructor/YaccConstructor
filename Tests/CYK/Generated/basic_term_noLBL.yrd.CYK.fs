namespace Yard.Generators.CYK

open Yard.Core
type cykToken = 
  | NUM
let getTag token = 
  match token with 
  | NUM -> 1
let rules = 
  [ 281479271677952u ]
  |> Array.ofList
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream |> Seq.map (fun t -> getTag t.Tag)