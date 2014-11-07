
# 2 "Mixed.yrd.fs"
module RNGLR.Mixed
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
type Token =
    | A of (int)
    | B of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "e"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | RNGLR_EOF _ -> 6

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 2; 3; 0; 0|]
let private rules = [|0; 4; 0; 4; 2; 2; 0; 5; 5|]
let private rulesStart = [|0; 2; 5; 6; 8; 9|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 6; 4; 2; 5; 3|]
let private small_gotos =
        [|3; 0; 131073; 327682; 65538; 262147; 327684; 131075; 0; 131077; 327682|]
let gotos = Array.zeroCreate 7
for i = 0 to 6 do
        gotos.[i] <- Array.zeroCreate 7
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
let private lists_reduces = [|[|0,2|]; [|1,3|]; [|4,1|]; [|3,2|]|]
let private small_reduces =
        [|131073; 393216; 196609; 393217; 262146; 262146; 327682; 327682; 262147; 327683|]
let reduces = Array.zeroCreate 7
for i = 0 to 6 do
        reduces.[i] <- Array.zeroCreate 7
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
let zeroReduces = Array.zeroCreate 7
for i = 0 to 6 do
        zeroReduces.[i] <- Array.zeroCreate 7
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
let private small_acc = [6]
let private accStates = Array.zeroCreate 7
for i = 0 to 6 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 6
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource
