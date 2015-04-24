
# 2 "SimpleBanch.yrd.fs"
module GLL.AbstractParse.SimpleBranch
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.AbstractParser
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_exp_brackets_1"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "C"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | C _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | B _ -> true
    | C _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | 7 -> true
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
    | s -> true
    | yard_exp_brackets_1 -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 3; 2; 2|]
let table = [| [||];[||];[||];[||];[|0|];[||];[||];[||];[||];[|2|];[|3|];[||];[|1|];[||];[||];[||]; |]
let private rules = [|4; 2; 1; 5; 6|]
let private canInferEpsilon = [|true; false; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree<Token>) -> tree.AstToDot numToString)

let private rulesStart = [|0; 2; 3; 4; 5|]
let startRule = 1
let indexatorFullCount = 8
let rulesCount = 4
let indexEOF = 7
let nonTermCount = 4
let termCount = 4
let termStart = 4
let termEnd = 7
let literalStart = 8
let literalEnd = 7
let literalsCount = 0

let slots = dict <| [|(-1, 0); (2, 1); (65537, 2)|]

let private parserSource = new ParserSourceGLL<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots)
let buildAbstractAst : (AbstractAnalysis.Common.ParserInputGraph<Token> -> ParseResult<_>) =
    buildAbstractAst<Token> parserSource


