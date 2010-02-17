module Yard.Core.ExpandAlter

open IL
open Rule
open Production
open Source
open System

let extract_one_rule (rule:Rule.t<'a,'b>) = 
    let rec expand = function
    |PAlt     (a,b) -> expand a  @ expand b
    |PSeq     (a,b) -> let wrap = List.map (fun x -> (x.rule, fun r -> {x with rule = r})) a
                                  |> List.unzip
                       in
                       let rec gen = function
                           | hd::tl -> [for x in hd -> x :: ( gen tl |> List.concat)]
                           | []     -> []
                       in 
                       fst wrap |> List.map expand |> gen 
                       |> List.map (fun x -> PSeq ((List.map2 ( |> ) x (snd wrap)),b))
    |PRef   _ 
    |PLiteral _
    |PToken   _ as t   -> [t]
    | _             -> (System.Console.WriteLine("incorrect tree for alternative expanding!")
                        ; failwith "incorrect tree for alternative expanding!")
    in 
    expand rule.body |> List.map (fun x -> {rule with body = x})