
# 2 "CalcErrorNonAmb.yrd.fs"
module RNGLR.ParseCalcErrorNonAmb
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | ADD of int
    | B of int
    | ERROR of int
    | LBRACE of int
    | MUL of int
    | RBRACE of int
    | RNGLR_EOF of int
    | SEMICOLON of int

let numToString = function
    | 0 -> "error"
    | 1 -> "expr"
    | 2 -> "fact"
    | 3 -> "list"
    | 4 -> "num"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "ADD"
    | 8 -> "B"
    | 9 -> "ERROR"
    | 10 -> "LBRACE"
    | 11 -> "MUL"
    | 12 -> "RBRACE"
    | 13 -> "RNGLR_EOF"
    | 14 -> "SEMICOLON"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 6
    | ADD _ -> 7
    | B _ -> 8
    | ERROR _ -> 9
    | LBRACE _ -> 10
    | MUL _ -> 11
    | RBRACE _ -> 12
    | RNGLR_EOF _ -> 13
    | SEMICOLON _ -> 14

let mutable private cur = 0
let leftSide = [|3; 3; 3; 5; 1; 1; 2; 2; 4; 4; 4; 4|]
let private rules = [|1; 9; 14; 3; 1; 14; 3; 3; 2; 1; 7; 1; 4; 2; 11; 2; 10; 9; 12; 10; 1; 12; 8; 6|]
let private rulesStart = [|0; 1; 4; 7; 8; 9; 12; 13; 16; 19; 22; 23; 24|]
let startRule = 3

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 20; 7; 8; 9; 17; 10; 2; 15; 3; 5; 6; 11; 13; 12; 14; 16; 18; 19|]
let private small_gotos =
        [|8; 65536; 131073; 196610; 262147; 393220; 524293; 589830; 655367; 65538; 458760; 917513; 131078; 65546; 131073; 262147; 393220; 524293; 655367; 196609; 458760; 262145; 720907; 327685; 131084; 262147; 393220; 524293; 655367; 393217; 720907; 655367; 65549; 131073; 262147; 393220; 524293; 589838; 655367; 720898; 458760; 786447; 851969; 786448; 983048; 65536; 131073; 196625; 262147; 393220; 524293; 589830; 655367; 1114113; 917522; 1179656; 65536; 131073; 196627; 262147; 393220; 524293; 589830; 655367|]
let gotos = Array.zeroCreate 21
for i = 0 to 20 do
        gotos.[i] <- Array.zeroCreate 15
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
let private lists_reduces = [|[|0,1|]; [|5,3|]; [|4,1|]; [|7,3|]; [|6,1|]; [|11,1|]; [|10,1|]; [|9,3|]; [|8,3|]; [|2,3|]; [|1,3|]|]
let private small_reduces =
        [|65537; 851968; 196612; 458753; 786433; 851969; 917505; 262148; 458754; 786434; 851970; 917506; 393221; 458755; 720899; 786435; 851971; 917507; 458757; 458756; 720900; 786436; 851972; 917508; 524293; 458757; 720901; 786437; 851973; 917509; 589829; 458758; 720902; 786438; 851974; 917510; 786437; 458759; 720903; 786439; 851975; 917511; 917509; 458760; 720904; 786440; 851976; 917512; 1048577; 851977; 1245185; 851978|]
let reduces = Array.zeroCreate 21
for i = 0 to 20 do
        reduces.[i] <- Array.zeroCreate 15
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 21
for i = 0 to 20 do
        zeroReduces.[i] <- Array.zeroCreate 15
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
let private small_acc = [20]
let private accStates = Array.zeroCreate 21
for i = 0 to 20 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 13
let errorNIndex = 0
let errorTIndex = 9
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorNIndex, errorTIndex)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


