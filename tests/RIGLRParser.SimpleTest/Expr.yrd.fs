module RIGLR.Expr
open Yard.Generators.RIGLR
open Yard.Generators.RIGLR.Parser
open Yard.Generators.Common.AST
type Token =
    | N of (int)
    | P of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | N x -> box x
    | P x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "e"
    | 1 -> "error"
    | 2 -> "yard_start_rule"
    | 3 -> "N"
    | 4 -> "P"
    | 5 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | N _ -> 3
    | P _ -> 4
    | RNGLR_EOF _ -> 5

let startRule = 2
let eofIndex = 5
let private rules = [|3; 7; 4; 7; 0|]
let private rulesStart = [|0; 1; 4; 5|]
let finalState = [4]
let popStates = set [|4|]
let leftSide = [|0; 0; 2|]
let epsilonRules = [||]

let table: (int*int)[][][] = 
    [|[|[||]; [|(1, 0)|]; [|(3, 2)|]|]; [|[||]; [||]; [|(4, 3)|]|]; [|[|(0, 4)|]; [||]; [||]|]; [|[||]; [|(5, 0)|]; [||]|]; [|[||]; [||]; [||]|]; 
      [|[|(1, 4)|]; [||]; [||]|]|]

let private parserSource = new ParserSource<Token> (table, tokenToNumber, genLiteral, numToString, tokenData, rules, rulesStart, leftSide, startRule, eofIndex, popStates, finalState)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


