module RIGLR.grammar7_7
open Yard.Generators.RIGLR
open Yard.Generators.RIGLR.Parser
open Yard.Generators.Common.AST
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
    | 0 -> "a"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "C"
    | 7 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | C _ -> 6
    | RNGLR_EOF _ -> 7

let startRule = 2
let eofIndex = 7
let private rules = [|0; 9; 6; 5; 2; 4|]
let private rulesStart = [|0; 3; 4; 5; 6; 6|]
let finalState = [5]
let popStates = set [|5|]
let leftSide = [|2; 2; 3; 0; 0|]
let epsilonRules = [|4|]

let table: (int*int)[][][] = 
    [|[|[|(4, 1)|]; [||]; [|(4, 2); (5, 3)|]|]; [|[||]; [|(4, 0)|]; [||]|]; [|[|(3, 1)|]; [||]; [||]|]; [|[|(1, 5)|]; [||]; [||]|]; [|[||]; [||]; [|(6, 6)|]|]; 
      [|[||]; [||]; [||]|]; [|[|(0, 5)|]; [||]; [||]|]|]

let private parserSource = new ParserSource<Token> (table, tokenToNumber, genLiteral, numToString, tokenData, rules, rulesStart, leftSide, startRule, eofIndex, popStates, finalState)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


