module Yard.Generators.CYK

open Yard.Core
open Yard.Generators.CYKGenerator
type cykToken = 
  | EOF
  | IDENT
  | STAR
  | FROM
  | AS
  | SELECT
let getTag token = 
  match token with 
  | EOF -> 0us
  | IDENT -> 1us
  | STAR -> 2us
  | FROM -> 3us
  | AS -> 4us
  | SELECT -> 5us
let rules = 
  [
  281483566841856UL; 1125908497104896UL; 1688854155231488UL; 1688879925559808UL;
  2533283380330496UL; 2533279085363456UL; 2533304855822848UL; 2814797012205568UL;
  2251847058784256UL; 1407413539045376UL; 3377755555495936UL; 844463585755136UL;
  3940705508917248UL; 3659187582140416UL; 3096241923686400UL; 1970329131941888UL;
  562971428257792UL ]
  |> Array.ofList
let StartNTerm = 1
let CodeTokenStream (stream:seq<CYKToken<cykToken,_>>) = 
  stream
  |> Seq.choose (fun t ->
    let tag = getTag t.Tag
    if tag <> 0us then Some tag else None)
  |> Array.ofSeq