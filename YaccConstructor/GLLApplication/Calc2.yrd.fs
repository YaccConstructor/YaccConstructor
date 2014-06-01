
# 2 "Calc2.yrd.fs"
module GLL.Calc2
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL.Parser
open Yard.Generators.GLL
open Yard.Generators.RNGLR.AST
type Token =
    | DIV of (int)
    | LBRACE of (int)
    | MINUS of (int)
    | MULT of (int)
    | NUMBER of (int)
    | PLUS of (int)
    | POW of (int)
    | RBRACE of (int)
    | RNGLR_EOF of (int)


let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | DIV x -> box x
    | LBRACE x -> box x
    | MINUS x -> box x
    | MULT x -> box x
    | NUMBER x -> box x
    | PLUS x -> box x
    | POW x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "factor"
    | 3 -> "factorOp"
    | 4 -> "powExpr"
    | 5 -> "powOp"
    | 6 -> "term"
    | 7 -> "termOp"
    | 8 -> "yard_rule_binExpr_1"
    | 9 -> "yard_rule_binExpr_3"
    | 10 -> "yard_rule_binExpr_5"
    | 11 -> "yard_rule_yard_many_1_2"
    | 12 -> "yard_rule_yard_many_1_4"
    | 13 -> "yard_rule_yard_many_1_6"
    | 14 -> "yard_start_rule"
    | 15 -> "DIV"
    | 16 -> "LBRACE"
    | 17 -> "MINUS"
    | 18 -> "MULT"
    | 19 -> "NUMBER"
    | 20 -> "PLUS"
    | 21 -> "POW"
    | 22 -> "RBRACE"
    | 23 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | DIV _ -> 15
    | LBRACE _ -> 16
    | MINUS _ -> 17
    | MULT _ -> 18
    | NUMBER _ -> 19
    | PLUS _ -> 20
    | POW _ -> 21
    | RBRACE _ -> 22
    | RNGLR_EOF _ -> 23

let isLiteral = function
    | DIV _ -> false
    | LBRACE _ -> false
    | MINUS _ -> false
    | MULT _ -> false
    | NUMBER _ -> false
    | PLUS _ -> false
    | POW _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false

let isTerminal = function
    | DIV _ -> true
    | LBRACE _ -> true
    | MINUS _ -> true
    | MULT _ -> true
    | NUMBER _ -> true
    | PLUS _ -> true
    | POW _ -> true
    | RBRACE _ -> true
    | RNGLR_EOF _ -> true
    | _ -> false

let numIsTerminal = function
    | 15 -> true
    | 16 -> true
    | 17 -> true
    | 18 -> true
    | 19 -> true
    | 20 -> true
    | 21 -> true
    | 22 -> true
    | 23 -> true
    | _ -> false

let numIsNonTerminal = function
    | 0 -> true
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | 4 -> true
    | 5 -> true
    | 6 -> true
    | 7 -> true
    | 8 -> true
    | 9 -> true
    | 10 -> true
    | 11 -> true
    | 12 -> true
    | 13 -> true
    | 14 -> true
    | _ -> false

let numIsLiteral = function
    | _ -> false

let isNonTerminal = function
    | error -> true
    | expr -> true
    | factor -> true
    | factorOp -> true
    | powExpr -> true
    | powOp -> true
    | term -> true
    | termOp -> true
    | yard_rule_binExpr_1 -> true
    | yard_rule_binExpr_3 -> true
    | yard_rule_binExpr_5 -> true
    | yard_rule_yard_many_1_2 -> true
    | yard_rule_yard_many_1_4 -> true
    | yard_rule_yard_many_1_6 -> true
    | yard_start_rule -> true
    | _ -> false

let getLiteralNames = []
let mutable private cur = 0

let acceptEmptyInput = false

let leftSide = [|1; 14; 8; 11; 11; 7; 7; 6; 9; 12; 12; 3; 3; 2; 10; 13; 13; 5; 4; 4|]
let table = [| [||];[||];[||];[||];[||];[||];[||];[||];[||];[||];[|0|];[||];[||];[|0|];[||];[||];[||];[||];[||];[|13|];[||];[||];[|13|];[||];[||];[||];[||];[|12|];[||];[||];[|11|];[||];[||];[||];[||];[||];[||];[|19|];[||];[||];[|18|];[||];[||];[||];[||];[||];[||];[||];[||];[||];[||];[|17|];[||];[||];[||];[|7|];[||];[||];[|7|];[||];[||];[||];[||];[||];[||];[|6|];[||];[||];[|5|];[||];[||];[||];[||];[|2|];[||];[||];[|2|];[||];[||];[||];[||];[||];[|8|];[||];[||];[|8|];[||];[||];[||];[||];[||];[|14|];[||];[||];[|14|];[||];[||];[||];[||];[||];[||];[|4|];[||];[||];[|4|];[||];[|4; 3|];[|4; 3|];[|10|];[||];[|10; 9|];[|10|];[||];[|10; 9|];[||];[|10; 9|];[|10; 9|];[|16; 15|];[||];[|16; 15|];[|16; 15|];[||];[|16; 15|];[|16|];[|16; 15|];[|16; 15|];[||];[|1|];[||];[||];[|1|];[||];[||];[||];[||]; |]
let private rules = [|8; 1; 6; 11; 7; 6; 11; 20; 17; 9; 2; 12; 3; 2; 12; 18; 15; 10; 4; 13; 5; 4; 13; 21; 19; 16; 1; 22|]
let private canInferEpsilon = [|true; false; false; false; false; false; false; false; false; false; false; true; true; true; false; false; false; false; false; false; false; false; false; false|]
let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private rulesStart = [|0; 1; 2; 4; 4; 7; 8; 9; 10; 12; 12; 15; 16; 17; 18; 20; 20; 23; 24; 25; 28|]
let startRule = 1
let indexatorFullCount = 24
let rulesCount = 20
let indexEOF = 23
let nonTermCount = 15
let termCount = 9
let termStart = 15
let termEnd = 23
let literalStart = 24
let literalEnd = 23
let literalsCount = 0


let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput,numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)
let buildAst : (seq<Token> -> ParseResult<_>) =
    buildAst<Token> parserSource


