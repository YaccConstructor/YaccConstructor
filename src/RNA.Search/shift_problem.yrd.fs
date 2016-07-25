
# 2 "shift_problem.yrd.fs"
module GLL.shift_problem
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
type Token =
    | A of (int)
    | C of (int)
    | G of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | C x -> box x
    | G x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "s2"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "C"
    | 6 -> "G"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | C _ -> 5
    | G _ -> 6
    | RNGLR_EOF _ -> 7

let isLiteral = function
    | A _ -> false
    | C _ -> false
    | G _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | C _ -> true
    | G _ -> true
    | RNGLR_EOF _ -> true

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

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = true

let leftSide = [|1; 3; 2; 2; 2|]
let table = new System.Collections.Generic.Dictionary<int, int[]>(10)
table.Add(65536, [|0|])
table.Add(65538, [|0|])
table.Add(65539, [|0|])
table.Add(196608, [|1|])
table.Add(196610, [|1|])
table.Add(196611, [|1|])
table.Add(131074, [|2|])
table.Add(131072, [|3|])
table.Add(131073, [|4|])
table.Add(131075, [|4|])

let private rules = [|2; 1; 7; 6; 2; 5; 2; 4; 4|]
let private canInferEpsilon = [|true; true; true; true; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree) -> tree.AstToDot numToString)

let private rulesStart = [|0; 1; 3; 7; 9; 9|]
let private probabilities = [|1.0; 1.0; 1.0; 1.0; 1.0|]
let startRule = 1
let indexatorFullCount = 8
let rulesCount = 5
let indexEOF = 7
let nonTermCount = 4
let termCount = 4
let termStart = 4
let termEnd = 7
let literalStart = 8
let literalEnd = 7
let literalsCount = 0

let slots = dict <| [|(-1, 0); (1, 1); (65537, 2); (131074, 3); (131076, 4)|]

let private parserSource = new ParserSourceGLL<Token> (Token.RNGLR_EOF 0, tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots, probabilities)

let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> int -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTree.buildAbstract<Token> parserSource


