
# 2 "CalcErrorAmb.yrd.fs"
module RNGLR.ParseCalcErrorAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | ADD of (int)
    | LBRACE of (int)
    | RBRACE of (int)
    | RNGLR_EOF of (int)
    | SEMICOLON of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | ADD x -> box x
    | LBRACE x -> box x
    | RBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMICOLON x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "list"
    | 3 -> "yard_opt_1"
    | 4 -> "yard_opt_2"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "ADD"
    | 8 -> "LBRACE"
    | 9 -> "RBRACE"
    | 10 -> "RNGLR_EOF"
    | 11 -> "SEMICOLON"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | ADD _ -> 7
    | LBRACE _ -> 8
    | RBRACE _ -> 9
    | RNGLR_EOF _ -> 10
    | SEMICOLON _ -> 11

let isLiteral = function
    | A _ -> false
    | ADD _ -> false
    | LBRACE _ -> false
    | RBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMICOLON _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 2; 5; 3; 3; 4; 4; 1; 1; 1; 1|]
let private rules = [|0; 4; 1; 3; 2; 11; 2; 11; 2; 6; 8; 0; 9; 8; 1; 9; 1; 7; 1|]
let private rulesStart = [|0; 2; 4; 5; 7; 7; 9; 9; 10; 13; 16; 19|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 17; 8; 9; 2; 3; 16; 5; 6; 14; 7; 10; 12; 11; 13; 15|]
let private small_gotos =
        [|5; 0; 65537; 131074; 393219; 524292; 65538; 262149; 720902; 196613; 0; 65537; 131079; 393219; 524292; 262147; 196616; 458761; 720906; 393219; 65547; 393219; 524292; 458753; 458761; 589828; 12; 65549; 393219; 524292; 655361; 589838; 786434; 458761; 589839; 917509; 0; 65537; 131088; 393219; 524292|]
let gotos = Array.zeroCreate 18
for i = 0 to 17 do
        gotos.[i] <- Array.zeroCreate 12
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
let private lists_reduces = [|[|0,1|]; [|0,2|]; [|1,1|]; [|1,2|]; [|10,3|]; [|7,1|]; [|8,3|]; [|9,3|]; [|3,2|]; [|5,2|]|]
let private small_reduces =
        [|65537; 655360; 131073; 655361; 262145; 655362; 327681; 655363; 458756; 458756; 589828; 655364; 720900; 524292; 458757; 589829; 655365; 720901; 720900; 458758; 589830; 655366; 720902; 851972; 458759; 589831; 655367; 720903; 983041; 655368; 1048577; 655369|]
let reduces = Array.zeroCreate 18
for i = 0 to 17 do
        reduces.[i] <- Array.zeroCreate 12
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
let private lists_zeroReduces = [|[|6|]; [|4|]|]
let private small_zeroReduces =
        [|65537; 655360; 262145; 655361|]
let zeroReduces = Array.zeroCreate 18
for i = 0 to 17 do
        zeroReduces.[i] <- Array.zeroCreate 12
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
let private small_acc = [17]
let private accStates = Array.zeroCreate 18
for i = 0 to 17 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 10
let errorIndex = 0
let errorRulesExists = true
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


