
# 2 "BadLeftRecursion.yrd.fs"
module RNGLR.BadLeftRecursion
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
type Token =
    | B of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | B x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "yard_start_rule"
    | 3 -> "B"
    | 4 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | B _ -> 3
    | RNGLR_EOF _ -> 4

let isLiteral = function
    | B _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 1; 1; 2|]
let private rules = [|1; 1; 1; 1; 1; 3; 1|]
let private rulesStart = [|0; 3; 5; 6; 7|]
let startRule = 3

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 4; 2; 3|]
let private small_gotos =
        [|2; 65536; 196609; 65538; 65538; 196609; 131074; 65539; 196609; 196610; 65539; 196609|]
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
let private lists_reduces = [|[|1,2|]; [|1,2; 0,3|]; [|2,1|]|]
let private small_reduces =
        [|131074; 196608; 262144; 196610; 196609; 262145; 262146; 196610; 262146|]
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
let eofIndex = 4
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource
