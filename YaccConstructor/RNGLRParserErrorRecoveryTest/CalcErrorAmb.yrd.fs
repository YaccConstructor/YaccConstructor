
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
    | EOF of int
    | ERROR of int
    | LBRACE of int
    | MUL of int
    | RBRACE of int
    | SEMICOLON of int

let numToString = function
    | 0 -> "expr"
    | 1 -> "list"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "ADD"
    | 5 -> "B"
    | 6 -> "EOF"
    | 7 -> "ERROR"
    | 8 -> "LBRACE"
    | 9 -> "MUL"
    | 10 -> "RBRACE"
    | 11 -> "SEMICOLON"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | ADD _ -> 4
    | B _ -> 5
    | EOF _ -> 6
    | ERROR _ -> 7
    | LBRACE _ -> 8
    | MUL _ -> 9
    | RBRACE _ -> 10
    | SEMICOLON _ -> 11

let mutable private cur = 0
let leftSide = [|1; 1; 1; 2; 0; 0; 0; 0; 0; 0|]
let private rules = [|0; 7; 11; 1; 0; 11; 1; 1; 8; 7; 10; 8; 0; 10; 0; 9; 0; 0; 4; 0; 5; 3|]
let private rulesStart = [|0; 1; 4; 7; 8; 11; 14; 17; 20; 21; 22|]
let startRule = 3

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 18; 6; 7; 15; 8; 2; 4; 13; 3; 5; 9; 11; 10; 12; 14; 16; 17|]
let private small_gotos =
        [|6; 0; 65537; 196610; 327683; 458756; 524293; 65539; 262150; 589831; 720904; 131076; 9; 196610; 327683; 524293; 196610; 262150; 589831; 262148; 10; 196610; 327683; 524293; 327682; 262150; 589831; 524293; 11; 196610; 327683; 458764; 524293; 589827; 262150; 589831; 655373; 720897; 655374; 851974; 0; 65551; 196610; 327683; 458756; 524293; 983041; 720912; 1048582; 0; 65553; 196610; 327683; 458756; 524293|]
let gotos = Array.zeroCreate 19
for i = 0 to 18 do
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
let private lists_reduces = [|[|0,1|]; [|7,3|]; [|6,3|]; [|9,1|]; [|8,1|]; [|5,3|]; [|4,3|]; [|2,3|]; [|1,3|]|]
let private small_reduces =
        [|65537; 393216; 196613; 262145; 393217; 589825; 655361; 720897; 327685; 262146; 393218; 589826; 655362; 720898; 393221; 262147; 393219; 589827; 655363; 720899; 458757; 262148; 393220; 589828; 655364; 720900; 655365; 262149; 393221; 589829; 655365; 720901; 786437; 262150; 393222; 589830; 655366; 720902; 917505; 393223; 1114113; 393224|]
let reduces = Array.zeroCreate 19
for i = 0 to 18 do
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 19
for i = 0 to 18 do
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
let private small_acc = [18]
let private accStates = Array.zeroCreate 19
for i = 0 to 18 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 6
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


