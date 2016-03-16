
# 2 "SimpleAmb.yrd.fs"
module GLL.SimpleAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.AbstractParserWithoutTree
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | D of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | D x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "cd"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "C"
    | 7 -> "D"
    | 8 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | C _ -> 6
    | D _ -> 7
    | RNGLR_EOF _ -> 8

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | D _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | B _ -> true
    | C _ -> true
    | D _ -> true
    | RNGLR_EOF _ -> true

let numIsTerminal = function
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | 7 -> true
    | 8 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|2; 3; 0|]
let table = new System.Collections.Generic.Dictionary<int, int[]>(3)
table.Add(131072, [|0|])
table.Add(196608, [|1|])
table.Add(2, [|2|])

let private rules = [|4; 5; 0; 0; 4; 2; 8; 6; 7|]
let private canInferEpsilon = [|false; true; false; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree<Token>) -> tree.AstToDot numToString)

let private rulesStart = [|0; 5; 7; 9|]
let private probabilities = [|1.0; 1.0; 1.0|]
let startRule = 1
let indexatorFullCount = 9
let rulesCount = 3
let indexEOF = 8
let nonTermCount = 4
let termCount = 5
let termStart = 4
let termEnd = 8
let literalStart = 9
let literalEnd = 8
let literalsCount = 0

let slots = dict <| [|(-1, 0); (3, 1); (4, 2); (65537, 3)|]

let private parserSource = new ParserSourceGLL<Token> (Token.RNGLR_EOF 0, tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots, probabilities)
let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph<Token> -> int -> int -> int -> ParserCommon.ParseResult<_>) =
    buildAbstract<Token> parserSource


