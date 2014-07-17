
# 2 "Cond.yrd.fs"
module GLL.ParseCond
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | ELSE of (int)
    | IF of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | ELSE x -> box x
    | IF x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "good"
    | 2 -> "if"
    | 3 -> "if_else"
    | 4 -> "s"
    | 5 -> "stmt"
    | 6 -> "yard_start_rule"
    | 7 -> "A"
    | 8 -> "ELSE"
    | 9 -> "IF"
    | 10 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 7
    | ELSE _ -> 8
    | IF _ -> 9
    | RNGLR_EOF _ -> 10

let isLiteral = function
    | A _ -> false
    | ELSE _ -> false
    | IF _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | ELSE _ -> true
    | IF _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 7 -> true
    | 8 -> true
    | 9 -> true
    | 10 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | error -> true
    | good -> true
    | if -> true
    | if_else -> true
    | s -> true
    | stmt -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|4; 6; 2; 2; 3; 1; 1; 5; 5|]
let table = [| [||];[||];[||];[||];[|5|];[||];[|6; 5|];[||];[||];[||];[|3; 2|];[||];[||];[||];[|4|];[||];[||];[||];[|0|];[||];[|7|];[||];[|8|];[||];[||];[||];[|1|];[||]; |]
let private rules = [|2; 4; 3; 9; 5; 9; 1; 8; 5; 5; 3; 7; 2|]
let private canInferEpsilon = [|true; false; false; false; false; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 1; 2; 3; 5; 9; 10; 11; 12; 13|]
let startRule = 1
let indexatorFullCount = 11
let rulesCount = 9
let indexEOF = 10
let nonTermCount = 7
let termCount = 4
let termStart = 7
let termEnd = 10
let literalStart = 11
let literalEnd = 10
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


