
# 2 "Mixed.yrd.fs"
module GLL.Mixed
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.Common.AST2
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
    | 0 -> "e"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | RNGLR_EOF _ -> 6

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
    | 4 -> true
    | 5 -> true
    | 6 -> true
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
    | e -> true
    | error -> true
    | s -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|2; 2; 3; 0; 0|]
let table = [| [||];[|4; 3|];[||];[||];[||];[||];[||];[|1; 0|];[||];[||];[|2|];[||]; |]
let private rules = [|0; 4; 0; 4; 2; 2; 0; 5; 5|]
let private canInferEpsilon = [|false; true; false; false; false; false; false|]
//let defaultAstToDot =
//    (fun (tree : Yard.Generators.Common.AST2.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 2; 5; 6; 8; 9|]
let startRule = 2
let indexatorFullCount = 7
let rulesCount = 5
let indexEOF = 6
let nonTermCount = 4
let termCount = 3
let termStart = 4
let termEnd = 6
let literalStart = 7
let literalEnd = 6
let literalsCount = 0

let slots = [(2,3);(2,3)] |> dict 

let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots)

let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


