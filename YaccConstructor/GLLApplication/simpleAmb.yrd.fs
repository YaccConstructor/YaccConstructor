
# 2 "SimpleAmb.yrd.fs"
module GLL.SimpleAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | D of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | D x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "D"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | D _ -> 5
    | RNGLR_EOF _ -> 6

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | D _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | B _ -> true
    | D _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 3 -> true
    | 4 -> true
    | 5 -> true
    | 6 -> true
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

let leftSide = [|1; 1; 1; 2|]
let table = [| [||];[||];[||];[||];[|2; 0|];[||];[|1|];[||];[|3|];[||];[|3|];[||]; |]
let private rules = [|3; 1; 4; 5; 3; 5; 4; 1|]
let private canInferEpsilon = [|true; false; false; false; false; false; false|]
let private rulesStart = [|0; 3; 4; 7; 8|]
let startRule = 3
let indexatorFullCount = 7
let rulesCount = 4
let indexEOF = 6
let nonTermCount = 3
let termCount = 4
let termStart = 3
let termEnd = 6
let literalStart = 7
let literalEnd = 6
let literalsCount = 0

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


