
# 2 "Emb.yrd.fs"
module GLL.AbstractParse.Emb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.AbstractParser
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
type Token =
    | A of (int)
    | LBR of (int)
    | RBR of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | LBR x -> box x
    | RBR x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "LBR"
    | 5 -> "RBR"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 3
    | LBR _ -> 4
    | RBR _ -> 5
    | RNGLR_EOF _ -> 6

let isLiteral = function
    | A _ -> false
    | LBR _ -> false
    | RBR _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | A _ -> true
    | LBR _ -> true
    | RBR _ -> true
    | RNGLR_EOF _ -> true

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

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 1; 2|]
let table = new System.Collections.Generic.Dictionary<int, int[]>(4)
table.Add(65537, [|0|])
table.Add(65536, [|1|])
table.Add(131072, [|2|])
table.Add(131073, [|2|])

let private rules = [|4; 1; 5; 3; 1; 6|]
let private canInferEpsilon = [|true; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree<Token>) -> tree.AstToDot numToString)

let private rulesStart = [|0; 3; 4; 6|]
let probs = [|1.0; 1.0; 1.0|]
let startRule = 2
let indexatorFullCount = 7
let rulesCount = 3
let indexEOF = 6
let nonTermCount = 3
let termCount = 4
let termStart = 3
let termEnd = 6
let literalStart = 7
let literalEnd = 6
let literalsCount = 0

let slots = dict <| [|(-1, 0); (2, 1); (131073, 2)|]

let parserSource = new ParserSourceGLL<Token> (Token.RNGLR_EOF 0, tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon, slots)
let buildAbstractAst : (AbstractAnalysis.Common.ParserInputGraph<Token> -> ParserCommon.ParseResult<_>) =
    buildAbstractAst<Token> parserSource


