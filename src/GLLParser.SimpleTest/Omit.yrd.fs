
# 2 "Omit.yrd.fs"
module GLL.ParseOmit
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
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
    | 0 -> "a"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_rule_list_1"
    | 4 -> "yard_rule_yard_many_1_2"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "B"
    | 8 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | B _ -> 7
    | RNGLR_EOF _ -> 8

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
    | 6 -> true
    | 7 -> true
    | 8 -> true
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
    | a -> true
    | error -> true
    | s -> true
    | yard_rule_list_1 -> true
    | yard_rule_yard_many_1_2 -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = true

let leftSide = [|2; 5; 3; 3; 4; 4; 0|]
let table = [| [|6|];[||];[||];[||];[||];[||];[|0|];[||];[|0|];[|3|];[||];[|3; 2|];[||];[|5|];[|5; 4|];[|1|];[||];[|1|]; |]
let private rules = [|3; 2; 0; 4; 7; 0; 4; 6|]
let private canInferEpsilon = [|false; true; true; true; true; true; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 1; 2; 2; 4; 4; 7; 8|]
let startRule = 1
let indexatorFullCount = 9
let rulesCount = 7
let indexEOF = 8
let nonTermCount = 6
let termCount = 3
let termStart = 6
let termEnd = 8
let literalStart = 9
let literalEnd = 8
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


