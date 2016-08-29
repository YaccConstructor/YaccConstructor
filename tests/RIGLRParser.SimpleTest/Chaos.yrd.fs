module RIGLR.Chaos
open Yard.Generators.RIGLR
open Yard.Generators.RIGLR.Parser
open Yard.Generators.Common.AST
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
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 3
    | RNGLR_EOF _ -> 4

let startRule = 3
let eofIndex = 4
let private rules = [|6; 6; 6; 6; 6; 3; 1|]
let private rulesStart = [|0; 2; 5; 6; 7|]
let finalState = [3]
let popStates = set [|3|]
let leftSide = [|1; 1; 1; 2|]
let epsilonRules = [||]

let table: (int*int)[][][] = 
    [|[|[||]; [|(2, 0)|]; [|(3, 1)|]|]; [|[|(2, 3)|]; [||]; [||]|]; [|[||]; [|(4, 0)|]; [||]|]; [|[||]; [||]; [||]|]; [|[|(0, 3)|]; [|(5, 0)|]; [||]|]; 
      [|[|(1, 3)|]; [||]; [||]|]|]

let private parserSource = new ParserSource<Token> (table, tokenToNumber, genLiteral, numToString, tokenData, rules, rulesStart, leftSide, startRule, eofIndex, popStates, finalState)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


