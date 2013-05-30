
# 2 "SimpleParser1.yrd.fs"
module AttendedRNGLR.Simple1
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | EMPTY of int
    | EOF of int

let numToString = function
    | 0 -> "e"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "EMPTY"
    | 5 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | EMPTY _ -> 4
    | EOF _ -> 5

let numToToken = function
    | 3 -> A 0
    | 4 -> EMPTY 0
    | 5 -> EOF 0
    | _ -> EOF 0 

let mutable private cur = 0
let leftSide = [|0; 0; 1; 2|]
let private rules = [|3; 0; 4; 0; 0; 1|]
let private rulesStart = [|0; 1; 4; 5; 6|]
let startRule = 3

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 5; 4; 2; 3|]
let private small_gotos =
        [|5; 0; 65537; 196610; 262146; 327682; 65539; 196611; 262147; 327683; 131076; 4; 196610; 262146; 327682; 196611; 196611; 262147; 327683|]
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
let attendedPushes = [|3; 4; 3; 4; 0; 0|]
let private lists_reduces = [|[|2,1|]; [|1,3|]; [|0,1|]|]
let private small_reduces =
        [|65537; 327680; 196611; 196609; 262145; 327681; 262147; 196610; 262146; 327682|]
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
let private small_acc = [5]
let private accStates = Array.zeroCreate 6
for i = 0 to 5 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 5
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, attendedPushes, numToToken)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


// Grammar analyzis 
// State 0 accepst only A symbol; 	 defalut: push and goto to [4] 
// State 1 accepst only EMPTY symbol; 	 defalut: push and goto to [2] 
// State 2 accepst only A symbol; 	 defalut: push and goto to [4] 
// State 3 accepst only EMPTY symbol; 	 defalut: push and goto to [2] 

