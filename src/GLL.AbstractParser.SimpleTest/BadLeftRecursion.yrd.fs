
# 2 "BadLeftRecursion.yrd.fs"
module GLL.AbstractParse.BadLeftRecursion
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.AbstractParser
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.AST3
type Token =
    | B of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | B x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "B"
    | 4 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | B _ -> 3
    | RNGLR_EOF _ -> 4

let isLiteral = function
    | B _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | B _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 3 -> true
    | 4 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | error -> true
    | s -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 1; 2|]
let table = [| [||];[||];[|1; 0|];[||];[|2|];[||]; |]
let private rules = [|1; 1; 3; 1|]
let private canInferEpsilon = [|true; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST3.Tree<Token>) -> tree.AstToDot numToString)

let private rulesStart = [|0; 2; 3; 4|]
let startRule = 2
let indexatorFullCount = 5
let rulesCount = 3
let indexEOF = 4
let nonTermCount = 3
let termCount = 2
let termStart = 3
let termEnd = 4
let literalStart = 5
let literalEnd = 4
let literalsCount = 0

let slots = dict <| [|(-1, 0); (1, 1); (2, 2); (131073, 3)|]

let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots)
let buildAbstractAst : (AbstractAnalysis.Common.ParserInputGraph<Token> -> ParseResult<_>) =
    buildAbstractAst<Token> parserSource


