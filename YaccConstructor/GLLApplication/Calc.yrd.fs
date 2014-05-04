
# 2 "Calc.yrd.fs"
module GLL.ParseCalc
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | RNGLR_EOF of (int)
    | L_star_ of (int)
    | L_plus_ of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | "*" -> Some (L_star_ data)
    | "+" -> Some (L_plus_ data)
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | RNGLR_EOF x -> box x
    | L_star_ x -> box x
    | L_plus_ x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "fact"
    | 3 -> "num"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "B"
    | 7 -> "RNGLR_EOF"
    | 8 -> "'*'"
    | 9 -> "'+'"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | B _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | RNGLR_EOF _ -> false
    | L_star_ _ -> true
    | L_plus_ _ -> true

let isTerminal = function
    | A _ -> true
    | B _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 5 -> true
    | 6 -> true
    | 7 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | 4 -> true
    | 5 -> true
    | _ -> false

let isNonTerminal = function
    | error -> true
    | expr -> true
    | fact -> true
    | num -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = ["*";"+";]
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 1; 4; 2; 2; 3; 3|]
let table = [| [||];[||];[||];[||];[||];[|1; 0|];[|1; 0|];[|0|];[||];[||];[|4; 3|];[|4; 3|];[||];[||];[||];[|5|];[|6|];[||];[||];[||];[|2|];[|2|];[||];[||];[||]; |]
let private rules = [|1; 9; 1; 2; 1; 2; 8; 2; 3; 5; 6|]
let private rulesStart = [|0; 3; 4; 5; 8; 9; 10; 11|]
let startRule = 2
let indexatorFullCount = 10
let rulesCount = 7
let indexEOF = 7
let nonTermCount = 5
let termCount = 3
let termStart = 5
let termEnd = 7
let literalStart = 8
let literalEnd = 9
let literalsCount = 2


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


