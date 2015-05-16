
# 2 "SimpleRightNull.yrd.fs"
module GLL.AbstractParse.SimpleRightNull
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.AbstractParser
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
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
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "t"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | RNGLR_EOF _ -> 5

let isLiteral = function
    | A _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 4 -> true
    | 5 -> true
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
    | t -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = true

let leftSide = [|1; 1; 3; 2|]
let table = [| [||];[||];[|0|];[|1|];[||];[|3|];[|2|];[|2|]; |]
let private rules = [|4; 1; 2; 1|]
let private canInferEpsilon = [|true; true; true; true; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree<Token>) -> tree.AstToDot numToString)

let private rulesStart = [|0; 3; 3; 4; 4|]
let startRule = 2
let indexatorFullCount = 6
let rulesCount = 4
let indexEOF = 5
let nonTermCount = 4
let termCount = 2
let termStart = 4
let termEnd = 5
let literalStart = 6
let literalEnd = 5
let literalsCount = 0

let slots = dict <| [|(-1, 0); (2, 1); (3, 2); (131073, 3)|]

let private parserSource = new ParserSourceGLL<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots)
let buildAbstractAst : (AbstractAnalysis.Common.ParserInputGraph<Token> -> ParseResult<_>) =
    buildAbstractAst<Token> parserSource


