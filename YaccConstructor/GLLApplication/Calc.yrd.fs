
# 2 "Calc.yrd.fs"
module GLL.ParseCalc
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | ADD x -> box x
    | B x -> box x
    | MUL x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "fact"
    | 3 -> "num"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "ADD"
    | 7 -> "B"
    | 8 -> "MUL"
    | 9 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | ADD _ -> 6
    | B _ -> 7
    | MUL _ -> 8
    | RNGLR_EOF _ -> 9

let isLiteral = function
    | A _ -> false
    | ADD _ -> false
    | B _ -> false
    | MUL _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 1; 4; 2; 2; 3; 3|]
let table = [|-1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; 0; -1; 0; -1; -1; -1; -1; -1; -1; -1; 3; -1; 3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1|]
let private rules = [|1; 6; 1; 2; 1; 2; 8; 2; 3; 5; 7|]
let private rulesStart = [|0; 3; 4; 5; 8; 9; 10; 11|]
let startRule = 2


let parserSource = new ParserSource<Token> (table, rules, rulesStart, leftSide, startRule, tokenToNumber, numToString)


