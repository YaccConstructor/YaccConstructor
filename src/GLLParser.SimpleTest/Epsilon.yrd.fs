
# 2 "Epsilon.yrd.fs"
module GLL.ParseEpsilon
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
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
    | 2 -> "yard_rule_op_1"
    | 3 -> "yard_rule_op_2"
    | 4 -> "yard_rule_op_3"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "B"
    | 8 -> "C"
    | 9 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | B _ -> 7
    | C _ -> 8
    | RNGLR_EOF _ -> 9

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
    | 6 -> true
    | 7 -> true
    | 8 -> true
    | 9 -> true
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
    | error -> true
    | s -> true
    | yard_rule_op_1 -> true
    | yard_rule_op_2 -> true
    | yard_rule_op_3 -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = true

let leftSide = [|1; 5; 4; 4; 3; 3; 2; 2|]
let table = [| [||];[||];[||];[||];[|0|];[|0|];[|0|];[|0|];[|6|];[|7; 6|];[|7; 6|];[|7; 6|];[||];[|4|];[|5; 4|];[|5; 4|];[||];[||];[|2|];[|3; 2|];[|1|];[|1|];[|1|];[|1|]; |]
let private rules = [|2; 3; 4; 1; 8; 7; 6|]
let private canInferEpsilon = [|true; true; true; true; true; true; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 3; 4; 5; 5; 6; 6; 7; 7|]
let startRule = 1
let indexatorFullCount = 10
let rulesCount = 8
let indexEOF = 9
let nonTermCount = 6
let termCount = 4
let termStart = 6
let termEnd = 9
let literalStart = 10
let literalEnd = 9
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


