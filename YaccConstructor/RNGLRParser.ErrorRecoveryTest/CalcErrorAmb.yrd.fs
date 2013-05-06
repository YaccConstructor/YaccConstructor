
# 2 "CalcErrorAmb.yrd.fs"
module RNGLR.ParseCalcErrorAmb
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
    | 2 -> "list"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "ADD"
    | 6 -> "B"
    | 7 -> "ERROR"
    | 8 -> "LBRACE"
    | 9 -> "MUL"
    | 10 -> "RBRACE"
    | 11 -> "RNGLR_EOF"
    | 12 -> "SEMICOLON"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 4
    | ADD _ -> 5
    | B _ -> 6
    | ERROR _ -> 7
    | LBRACE _ -> 8
    | MUL _ -> 9
    | RBRACE _ -> 10
    | RNGLR_EOF _ -> 11
    | SEMICOLON _ -> 12

let mutable private cur = 0
let leftSide = [|2; 2; 2; 3; 1; 1; 1; 1; 1; 1|]
let private rules = [|1; 7; 12; 2; 1; 12; 2; 2; 8; 7; 10; 8; 1; 10; 1; 9; 1; 1; 5; 1; 6; 4|]
let private rulesStart = [|0; 1; 4; 7; 8; 11; 14; 17; 20; 21; 22|]
let startRule = 3

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 18; 6; 7; 15; 8; 2; 4; 13; 3; 5; 9; 11; 10; 12; 14; 16; 17|]
let private small_gotos =
        [|6; 65536; 131073; 262146; 393219; 458756; 524293; 65539; 327686; 589831; 786440; 131076; 65545; 262146; 393219; 524293; 196610; 327686; 589831; 262148; 65546; 262146; 393219; 524293; 327682; 327686; 589831; 524293; 65547; 262146; 393219; 458764; 524293; 589827; 327686; 589831; 655373; 720897; 655374; 851974; 65536; 131087; 262146; 393219; 458756; 524293; 983041; 786448; 1048582; 65536; 131089; 262146; 393219; 458756; 524293|]
let gotos = Array.zeroCreate 19
for i = 0 to 18 do
        gotos.[i] <- Array.zeroCreate 13
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
let private lists_reduces = [|[|0,1|]; [|7,3|]; [|6,3|]; [|9,1|]; [|8,1|]; [|5,3|]; [|4,3|]; [|2,3|]; [|1,3|]|]
let private small_reduces =
        [|65537; 720896; 196613; 327681; 589825; 655361; 720897; 786433; 327685; 327682; 589826; 655362; 720898; 786434; 393221; 327683; 589827; 655363; 720899; 786435; 458757; 327684; 589828; 655364; 720900; 786436; 655365; 327685; 589829; 655365; 720901; 786437; 786437; 327686; 589830; 655366; 720902; 786438; 917505; 720903; 1114113; 720904|]
let reduces = Array.zeroCreate 19
for i = 0 to 18 do
        reduces.[i] <- Array.zeroCreate 13
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
let zeroReduces = Array.zeroCreate 19
for i = 0 to 18 do
        zeroReduces.[i] <- Array.zeroCreate 13
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
let private small_acc = [18]
let private accStates = Array.zeroCreate 19
for i = 0 to 18 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 11
let errorNIndex = 0
let errorTIndex = 7
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorNIndex, errorTIndex)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


