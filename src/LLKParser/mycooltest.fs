module LLk.test
open System.Collections.Generic
open Yard.Generators.LLK.Parser
open System
let tokenToNumber = function 
    | "A" -> 5
    | "C" -> 8
    | "D" -> 10
    | "B" -> 11
    | _   -> failwith "Unexpected token"
let isTerminal = function 
    | "A" -> true
    | "C" -> true
    | "D" -> true
    | "B" -> true
    | _ -> false
let startNT = 1
let k = 2
let numIsTerminal = function
    | 5 -> true
    | 8 -> true
    | 10 -> true
    | 11 -> true
    | _ -> false
let chainToNum = function
    | "5$5$" -> 0
    | "5$8$" -> 1
    | "5$10$" -> 2
    | "5$11$" -> 3
    | "5$" -> 4
    | "8$" -> 5
    | "10$" -> 6
    | "8$5$" -> 7
    | "10$5$" -> 8
    | "11$8$" -> 9
    | "11$10$" -> 10
    | _ -> -1 
let table = [|
  [|0;0;0;0;0;0;0;0;0;0;0;0;|];
  [|1;2;2;0;0;0;0;0;0;0;0;0;|];
  [|10;10;10;10;10;0;0;0;0;0;0;0;|];
  [|0;8;8;8;0;0;0;0;0;0;0;0;|];
  [|0;0;0;0;0;0;0;7;7;0;0;0;|];
  [|0;0;0;0;0;0;0;0;0;0;0;0;|];
  [|0;3;3;4;0;0;0;0;0;0;0;0;|];
  [|0;0;0;0;0;0;0;0;0;9;9;0;|];
  [|0;0;0;0;0;0;0;0;0;0;0;0;|];
  [|0;0;0;0;0;5;6;5;6;0;0;0;|];
  [|0;0;0;0;0;0;0;0;0;0;0;0;|];
  [|0;0;0;0;0;0;0;0;0;0;0;0;|];
|]
let rules = [|
    [|1;2;3|];
    [|1;2;4|];
    [|6;5;0|];
    [|6;2;7|];
    [|9;8;0|];
    [|9;10;0|];
    [|4;9;2|];
    [|3;6;9|];
    [|7;11;0|];
    [|2;5;0|];
|]
let parser = new Parser(tokenToNumber, isTerminal, table, startNT, numIsTerminal, k, chainToNum, rules)