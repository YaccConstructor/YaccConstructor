
# 2 "TrivialRecovery.yrd.fs"
module RNGLR.ParseTrivialRecovery
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | EOF of int
    | SEMICOLON of int

let numToString = function
    | 0 -> "a"
    | 1 -> "yard_start_rule"
    | 2 -> "A"
    | 3 -> "B"
    | 4 -> "EOF"
    | 5 -> "SEMICOLON"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 2
    | B _ -> 3
    | EOF _ -> 4
    | SEMICOLON _ -> 5

let mutable private cur = 0
let leftSide = [|0; 0; 1|]
let private rules = [|2; 5; 0; 3; 0|]
let private rulesStart = [|0; 3; 4; 5|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 5; 3; 4|]
let private small_gotos =
        [|3; 0; 131073; 196610; 131073; 327683; 196611; 4; 131073; 196610|]
let gotos = Array.zeroCreate 6
for i = 0 to 5 do
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
let private lists_reduces = [|[|0,3|]; [|1,1|]|]
let private small_reduces =
        [|262145; 262144; 327681; 262145|]
let reduces = Array.zeroCreate 6
for i = 0 to 5 do
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 6
for i = 0 to 5 do
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
let private small_acc = [1]
let private accStates = Array.zeroCreate 6
for i = 0 to 5 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 4
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


