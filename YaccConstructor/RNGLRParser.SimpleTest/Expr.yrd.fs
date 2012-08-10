module RNGLR.ParseExpr
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | EOF of int
    | N of int
    | P of int

let numToString = function 
    | 0 -> "e"
    | 1 -> "yard_start_rule"
    | 2 -> "EOF"
    | 3 -> "N"
    | 4 -> "P"
    | _ -> ""
let tokenToNumber = function
    | EOF _ -> 2
    | N _ -> 3
    | P _ -> 4

let mutable private cur = 0
let leftSide = [|0; 0; 1|]
let private rules = [|3; 0; 4; 0; 0|]
let private rulesStart = [|0; 1; 4; 5|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot = 
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 2; 3|]
let private small_gotos =
        [|2; 0; 196609; 65537; 262146; 131074; 3; 196609; 196609; 262146|]
let gotos = Array.zeroCreate 5
for i = 0 to 4 do
        gotos.[i] <- Array.zeroCreate 5
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
let private lists_reduces = [|[|1,3|]; [|0,1|]|]
let private small_reduces =
        [|196610; 131072; 262144; 262146; 131073; 262145|]
let reduces = Array.zeroCreate 5
for i = 0 to 4 do
        reduces.[i] <- Array.zeroCreate 5
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
let zeroReduces = Array.zeroCreate 5
for i = 0 to 4 do
        zeroReduces.[i] <- Array.zeroCreate 5
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
let private accStates = Array.zeroCreate 5
for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 2
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

