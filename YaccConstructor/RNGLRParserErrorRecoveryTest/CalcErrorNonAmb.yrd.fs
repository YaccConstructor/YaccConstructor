
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
    | EOF of int
    | ERROR of int
    | LBRACE of int
    | MUL of int
    | RBRACE of int
    | SEMICOLON of int

let numToString = function
    | 0 -> "expr"
    | 1 -> "fact"
    | 2 -> "list"
    | 3 -> "num"
    | 4 -> "yard_start_rule"
    | 5 -> "A"
    | 6 -> "ADD"
    | 7 -> "B"
    | 8 -> "EOF"
    | 9 -> "ERROR"
    | 10 -> "LBRACE"
    | 11 -> "MUL"
    | 12 -> "RBRACE"
    | 13 -> "SEMICOLON"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 5
    | ADD _ -> 6
    | B _ -> 7
    | EOF _ -> 8
    | ERROR _ -> 9
    | LBRACE _ -> 10
    | MUL _ -> 11
    | RBRACE _ -> 12
    | SEMICOLON _ -> 13

let mutable private cur = 0
let leftSide = [|2; 2; 2; 4; 0; 0; 1; 1; 3; 3; 3; 3|]
let private rules = [|0; 9; 13; 2; 0; 13; 2; 2; 1; 0; 6; 0; 3; 1; 11; 1; 10; 9; 12; 10; 0; 12; 7; 5|]
let private rulesStart = [|0; 1; 4; 7; 8; 9; 12; 13; 16; 19; 22; 23; 24|]
let startRule = 3

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 20; 7; 8; 9; 17; 10; 2; 15; 3; 5; 6; 11; 13; 12; 14; 16; 18; 19|]
let private small_gotos =
        [|8; 0; 65537; 131074; 196611; 327684; 458757; 589830; 655367; 65538; 393224; 851977; 131078; 10; 65537; 196611; 327684; 458757; 655367; 196609; 393224; 262145; 720907; 327685; 65548; 196611; 327684; 458757; 655367; 393217; 720907; 655367; 13; 65537; 196611; 327684; 458757; 589838; 655367; 720898; 393224; 786447; 851969; 786448; 983048; 0; 65537; 131089; 196611; 327684; 458757; 589830; 655367; 1114113; 851986; 1179656; 0; 65537; 131091; 196611; 327684; 458757; 589830; 655367|]
let gotos = Array.zeroCreate 21
for i = 0 to 20 do
        gotos.[i] <- Array.zeroCreate 14
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
        [|65537; 524288; 196612; 393217; 524289; 786433; 851969; 262148; 393218; 524290; 786434; 851970; 393221; 393219; 524291; 720899; 786435; 851971; 458757; 393220; 524292; 720900; 786436; 851972; 524293; 393221; 524293; 720901; 786437; 851973; 589829; 393222; 524294; 720902; 786438; 851974; 786437; 393223; 524295; 720903; 786439; 851975; 917509; 393224; 524296; 720904; 786440; 851976; 1048577; 524297; 1245185; 524298|]
let reduces = Array.zeroCreate 21
for i = 0 to 20 do
        reduces.[i] <- Array.zeroCreate 14
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
        zeroReduces.[i] <- Array.zeroCreate 14
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
let eofIndex = 8
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


