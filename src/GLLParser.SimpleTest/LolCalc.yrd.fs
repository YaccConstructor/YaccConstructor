
# 2 "LolCalc.yrd.fs"
module GLL.ParseLolCalc
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | ADD of (int)
    | B of (int)
    | MUL of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | ADD x -> box x
    | B x -> box x
    | MUL x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "num"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "ADD"
    | 6 -> "B"
    | 7 -> "MUL"
    | 8 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | ADD _ -> 5
    | B _ -> 6
    | MUL _ -> 7
    | RNGLR_EOF _ -> 8

let isLiteral = function
    | A _ -> false
    | ADD _ -> false
    | B _ -> false
    | MUL _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | ADD _ -> true
    | B _ -> true
    | MUL _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | 7 -> true
    | 8 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | error -> true
    | expr -> true
    | num -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 1; 1; 3; 2; 2|]
let table = [| [||];[||];[||];[||];[||];[|2; 1; 0|];[||];[|2; 1; 0|];[||];[||];[|4|];[||];[|5|];[||];[||];[|3|];[||];[|3|];[||];[||]; |]
let private rules = [|1; 5; 1; 1; 7; 1; 2; 1; 4; 6|]
let private canInferEpsilon = [|true; false; false; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 3; 6; 7; 8; 9; 10|]
let startRule = 3
let indexatorFullCount = 9
let rulesCount = 6
let indexEOF = 8
let nonTermCount = 4
let termCount = 5
let termStart = 4
let termEnd = 8
let literalStart = 9
let literalEnd = 8
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


