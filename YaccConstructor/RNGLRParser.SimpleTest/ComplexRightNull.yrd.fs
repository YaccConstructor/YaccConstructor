
# 2 "ComplexRightNull.yrd.fs"
module RNGLR.ParseComplexRightNull
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function
    | 0 -> "s"
    | 1 -> "t"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | EOF _ -> 5

let mutable private cur = 0
let leftSide = [|0; 0; 0; 2; 1|]
let private rules = [|3; 4; 1; 0; 0; 0|]
let private rulesStart = [|0; 0; 1; 5; 6; 6|]
let startRule = 3

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 4; 5; 7; 8; 6; 9; 10; 11|]
let private small_gotos =
        [|3; 0; 196609; 262146; 196609; 65539; 262147; 4; 196613; 262150; 327683; 7; 196609; 262146; 524289; 65544; 589827; 9; 196613; 262150; 655363; 10; 196613; 262150|]
let gotos = Array.zeroCreate 12
for i = 0 to 11 do
        gotos.[i] <- Array.zeroCreate 6
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
let private lists_reduces = [|[|1,1|]; [|2,1|]; [|2,2|]; [|2,3|]; [|2,4|]|]
let private small_reduces =
        [|131073; 327680; 196609; 327681; 262145; 327682; 327681; 327683; 393217; 327684; 458755; 196608; 262144; 327680; 524291; 196609; 262145; 327681; 589827; 196610; 262146; 327682; 655363; 196611; 262147; 327683; 720899; 196612; 262148; 327684|]
let reduces = Array.zeroCreate 12
for i = 0 to 11 do
        reduces.[i] <- Array.zeroCreate 6
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
let private lists_zeroReduces = [|[|3; 0|]; [|4|]; [|0|]|]
let private small_zeroReduces =
        [|1; 327680; 196611; 196609; 262145; 327681; 262147; 196610; 262146; 327682; 327681; 327682; 524291; 196609; 262145; 327681; 589827; 196610; 262146; 327682; 655363; 196610; 262146; 327682|]
let zeroReduces = Array.zeroCreate 12
for i = 0 to 11 do
        zeroReduces.[i] <- Array.zeroCreate 6
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
let private small_acc = [1; 0]
let private accStates = Array.zeroCreate 12
for i = 0 to 11 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 5
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


