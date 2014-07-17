
# 2 "LongCycle.yrd.fs"
module GLL.ParseLongCycle
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "e"
    | 1 -> "error"
    | 2 -> "red"
    | 3 -> "start"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | RNGLR_EOF _ -> 6

let isLiteral = function
    | A _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 5 -> true
    | 6 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | 4 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | e -> true
    | error -> true
    | red -> true
    | start -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|3; 4; 0; 0; 2|]
let table = [| [|3; 2|];[||];[||];[||];[|4|];[||];[|0|];[||];[|1|];[||]; |]
let private rules = [|0; 3; 5; 2; 0|]
let private canInferEpsilon = [|false; true; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 1; 2; 3; 4; 5|]
let startRule = 1
let indexatorFullCount = 7
let rulesCount = 5
let indexEOF = 6
let nonTermCount = 5
let termCount = 2
let termStart = 5
let termEnd = 6
let literalStart = 7
let literalEnd = 6
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


