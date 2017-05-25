module GLL.Parse.test
open AbstractAnalysis.Common
open Yard.Generators.OldGLL
open Yard.Generators.Common.ASTGLL
open Yard.Generators.OldGLL.ParserCommon
type Token =
    | A of (unit)
    | RNGLR_EOF of (unit)

let numToString = function
    | 0 -> "error"
    | 1 -> "k"
    | 2 -> "s"
    | 3 -> "yard_exp_brackets_1"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 5
    | RNGLR_EOF _ -> 6

let numIsTerminal = function
    | 5 -> true
    | 6 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let mutable private cur = 0

let leftSide = [|2; 4; 1; 1; 1; 3; 3|]
let table = new System.Collections.Generic.Dictionary<int, int[]>(4)
table.Add(131072, [|0|])
table.Add(262144, [|1|])
table.Add(65536, [|2;3;4|])
table.Add(196608, [|5;6|])

let private rules = [|1; 3; 2; 6; 2; 1; 5; 1; 5; 1; 1; 1; 1; 1; 5; 1; 1; 1; 1|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.ASTGLL.Tree) -> tree.AstToDot numToString)

let private rulesStart = [|0; 2; 4; 6; 8; 9; 14; 19|]
let startRule = 1
let rulesCount = 7
let nonTermCount = 5

let slots = dict <| [|(-1, 0); (1, 1); (2, 2); (65537, 3); (131073, 4); (131074, 5); (196610, 6); (327681, 7); (327682, 8); (327683, 9); (327684, 10); (327685, 11); (393218, 12); (393219, 13); (393220, 14); (393221, 15)|]

let private parserSource = new ParserSourceGLL<Token> (tokenToNumber, numToString, table, rules, rulesStart, leftSide, startRule, nonTermCount, rulesCount,numIsTerminal, numIsLiteral, slots)
let buildAbstract : (IParserInput -> int -> Yard.Generators.OldGLL.AbstractParserWithoutTree.ResultStruct []) =
    Yard.Generators.OldGLL.AbstractParserWithoutTree.buildAbstract<Token> parserSource


