//this tables was generated by GNESCC
//source grammar:../../../Tests/GNESCC/recursive_rules/test_rec_rule_3/test_rec_rule_3.yrd
//date:06.12.2011 22:03:00

module Yard.Generators.GNESCCGenerator.Tables_test_rec_rule_3

open Yard.Generators.GNESCCGenerator
open Yard.Generators.GNESCCGenerator.CommonTypes

type symbol =
    | T_PLUS
    | NT_s
    | NT_gnesccStart
let getTag smb =
    match smb with
    | T_PLUS -> 5
    | NT_s -> 4
    | NT_gnesccStart -> 2
let getName tag =
    match tag with
    | 5 -> T_PLUS
    | 4 -> NT_s
    | 2 -> NT_gnesccStart
    | _ -> failwith "getName: bad tag."
let prodToNTerm = 
  [| 1; 0 |];
let symbolIdx = 
  [| 1; 2; 1; 3; 0; 0 |];
let startKernelIdxs =  [0]
let isStart =
  [| [| true; true |];
     [| false; false |];
     [| false; true |];
     [| false; false |];
     [| false; false |]; |]
let gotoTable =
  [| [| Some 1; None |];
     [| None; None |];
     [| Some 3; None |];
     [| None; None |];
     [| None; None |]; |]
let actionTable = 
  [| [| [Shift 2]; [Error]; [Error] |];
     [| [Accept]; [Accept]; [Accept] |];
     [| [Shift 2]; [Error]; [Error] |];
     [| [Shift 4]; [Error]; [Error] |];
     [| [Reduce 1]; [Reduce 1]; [Reduce 1] |]; |]
let tables = 
  {StartIdx=startKernelIdxs
   SymbolIdx=symbolIdx
   GotoTable=gotoTable
   ActionTable=actionTable
   IsStart=isStart
   ProdToNTerm=prodToNTerm}
