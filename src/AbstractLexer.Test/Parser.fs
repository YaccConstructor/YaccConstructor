
# 2 "Parser.fs"
module AbstractLexer.Test.Calc.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "calc.yrd"

open AbstractLexer.Core

# 13 "Parser.fs"
type Token =
    | DIV of (string*array<Position<string>>)
    | LBRACE of (string*array<Position<string>>)
    | MINUS of (string*array<Position<string>>)
    | MULT of (string*array<Position<string>>)
    | NUMBER of (string*array<Position<string>>)
    | PLUS of (string*array<Position<string>>)
    | POW of (string*array<Position<string>>)
    | RBRACE of (string*array<Position<string>>)
    | RNGLR_EOF of (string*array<Position<string>>)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
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

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 14; 8; 11; 11; 7; 7; 6; 9; 12; 12; 3; 3; 2; 10; 13; 13; 5; 4; 4|]
let private rules = [|8; 1; 6; 11; 7; 6; 11; 17; 20; 9; 2; 12; 3; 2; 12; 15; 18; 10; 4; 13; 5; 4; 13; 21; 16; 1; 22; 19|]
let private rulesStart = [|0; 1; 2; 4; 7; 7; 8; 9; 10; 12; 15; 15; 16; 17; 18; 20; 23; 23; 24; 27; 28|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 20; 28; 26; 18; 13; 16; 3; 19; 6; 7; 4; 5; 9; 17; 12; 10; 11; 14; 15; 21; 27; 24; 25; 22; 23|]
let private small_gotos =
        [|9; 65536; 131073; 262146; 393219; 524292; 589829; 655366; 1048583; 1245192; 131076; 196617; 786442; 983051; 1179660; 196613; 131085; 262146; 655366; 1048583; 1245192; 262148; 196617; 786446; 983051; 1179660; 524291; 327695; 851984; 1376273; 589827; 262162; 1048583; 1245192; 655363; 327695; 851987; 1376273; 851977; 65556; 131073; 262146; 393219; 524292; 589829; 655366; 1048583; 1245192; 917505; 1441813; 1310724; 458774; 720919; 1114136; 1310745; 1376263; 131073; 262146; 393242; 589829; 655366; 1048583; 1245192; 1441796; 458774; 720923; 1114136; 1310745|]
let gotos = Array.zeroCreate 29
for i = 0 to 28 do
        gotos.[i] <- Array.zeroCreate 24
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|8,1|]; [|9,2|]; [|9,3|]; [|11,1|]; [|12,1|]; [|14,1|]; [|15,2|]; [|15,3|]; [|17,1|]; [|18,3|]; [|19,1|]; [|14,2|]; [|13,1|]; [|8,2|]; [|2,1|]; [|3,2|]; [|3,3|]; [|5,1|]; [|6,1|]; [|7,1|]; [|2,2|]; [|0,1|]|]
let private small_reduces =
        [|131076; 1114112; 1310720; 1441792; 1507328; 262148; 1114113; 1310721; 1441793; 1507329; 327684; 1114114; 1310722; 1441794; 1507330; 393218; 1048579; 1245187; 458754; 1048580; 1245188; 524294; 983045; 1114117; 1179653; 1310725; 1441797; 1507333; 655366; 983046; 1114118; 1179654; 1310726; 1441798; 1507334; 720902; 983047; 1114119; 1179655; 1310727; 1441799; 1507335; 786434; 1048584; 1245192; 983047; 983049; 1114121; 1179657; 1310729; 1376265; 1441801; 1507337; 1048583; 983050; 1114122; 1179658; 1310730; 1376266; 1441802; 1507338; 1114118; 983051; 1114123; 1179659; 1310731; 1441803; 1507339; 1179654; 983052; 1114124; 1179660; 1310732; 1441804; 1507340; 1245188; 1114125; 1310733; 1441805; 1507341; 1310722; 1441806; 1507342; 1441794; 1441807; 1507343; 1507330; 1441808; 1507344; 1572866; 1048593; 1245201; 1638402; 1048594; 1245202; 1703940; 1114131; 1310739; 1441811; 1507347; 1769474; 1441812; 1507348; 1835010; 1441813; 1507349|]
let reduces = Array.zeroCreate 29
for i = 0 to 28 do
        reduces.[i] <- Array.zeroCreate 24
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|10|]; [|16|]; [|4|]|]
let private small_zeroReduces =
        [|131076; 1114112; 1310720; 1441792; 1507328; 262148; 1114112; 1310720; 1441792; 1507328; 524294; 983041; 1114113; 1179649; 1310721; 1441793; 1507329; 655366; 983041; 1114113; 1179649; 1310721; 1441793; 1507329; 1310722; 1441794; 1507330; 1441794; 1441794; 1507330|]
let zeroReduces = Array.zeroCreate 29
for i = 0 to 28 do
        zeroReduces.[i] <- Array.zeroCreate 24
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [1]
let private accStates = Array.zeroCreate 29
for i = 0 to 28 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 23
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource


