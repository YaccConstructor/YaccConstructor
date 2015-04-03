
# 2 "SimpleAmb.yrd.fs"
module GLL.Parse.SimpleAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.Common.AST3
type Token =
    | A of (int)
    | B of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "s"
    | 3 -> "yard_many_1"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "B"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | B _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | RNGLR_EOF _ -> false

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
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | error -> true
    | expr -> true
    | s -> true
    | yard_many_1 -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|2; 4; 1; 3; 3|]
let table = [| [||];[||];[||];[||];[|2|];[|2|];[|0|];[||];[||];[||];[|4|];[|3|];[|1|];[||];[||]; |]
let private rules = [|5; 1; 2; 3; 6; 3|]
let private canInferEpsilon = [|true; true; false; true; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST3.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 2; 3; 4; 4; 6|]
let startRule = 1
let indexatorFullCount = 8
let rulesCount = 5
let indexEOF = 7
let nonTermCount = 5
let termCount = 3
let termStart = 5
let termEnd = 7
let literalStart = 8
let literalEnd = 7
let literalsCount = 0

let slots = dict <| [|(-1, 0); (2, 1); (65537, 2); (131073, 3); (262146, 4)|]

let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


