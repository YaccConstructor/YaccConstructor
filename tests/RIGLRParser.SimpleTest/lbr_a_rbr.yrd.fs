module RIGLR.Brackets
open Yard.Generators.RIGLR
open Yard.Generators.RIGLR.Parser
open Yard.Generators.Common.AST
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

let startRule = 3
let eofIndex = 6
let private rules = [|4; 8; 8; 5; 3; 1|]
let private rulesStart = [|0; 4; 5; 5; 6|]
let finalState = [1]
let popStates = set [|1|]
let leftSide = [|1; 1; 1; 2|]
let epsilonRules = [|2|]

let table: (int*int)[][][] = 
    [|[|[|(2, 1)|]; [||]; [|(3, 2); (4, 3)|]|]; [|[||]; [||]; [||]|]; [|[|(1, 1)|]; [||]; [||]|]; [|[||]; [|(4, 0)|]; [||]|]; [|[||]; [|(5, 0)|]; [||]|]; 
      [|[||]; [||]; [|(5, 6)|]|]; [|[|(0, 1)|]; [||]; [||]|]|]

let private parserSource = new ParserSource<Token> (table, tokenToNumber, genLiteral, numToString, tokenData, rules, rulesStart, leftSide, startRule, eofIndex, popStates, finalState)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


