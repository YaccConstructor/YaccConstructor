
# 2 "Order.yrd.fs"
module GLL.ParseOrder
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST

# 1 "Order.yrd"

let res = ref []

# 13 "Order.yrd.fs"
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
    | 0 -> "e1"
    | 1 -> "e2"
    | 2 -> "e3"
    | 3 -> "error"
    | 4 -> "s"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
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

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | e1 -> true
    | e2 -> true
    | e3 -> true
    | error -> true
    | s -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|4; 5; 0; 1; 2|]
let table = [| [|2|];[||];[|3|];[||];[|4|];[||];[||];[||];[|0|];[||];[|1|];[||]; |]
let private rules = [|0; 0; 4; 1; 1; 2; 2; 6|]
let private canInferEpsilon = [|false; false; false; true; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 2; 3; 5; 7; 8|]
let startRule = 1
let indexatorFullCount = 8
let rulesCount = 5
let indexEOF = 7
let nonTermCount = 6
let termCount = 2
let termStart = 6
let termEnd = 7
let literalStart = 8
let literalEnd = 7
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


