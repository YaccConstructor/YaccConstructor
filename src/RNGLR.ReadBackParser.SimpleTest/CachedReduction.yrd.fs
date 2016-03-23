
# 2 "CachedReduction.yrd.fs"
module RNGLR.ReadBackParser.CachedReduction
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.ReadBack.Parser
open Yard.Generators.RNGLR.ReadBack
type Token =
    | A of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "a"
    | 1 -> "error"
    | 2 -> "s"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | RNGLR_EOF _ -> 5

let indexToSymbolType = function
    | i when i >= 0 && i < 4 ->
        SymbolType.Nonterminal
    | i when i >= 4 && i <= 5 ->
        SymbolType.Terminal
    | i when i >= 6 && i <= 5 ->
        SymbolType.Terminal
    | i when i = 6 ->
        SymbolType.Epsilon
    | _ ->
        invalidArg "index" "" |> raise
let isLiteral = function
    | A _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|0; 3; 2|]
let private nfas = [|4, [0, [1, 6; 3, 6]; 1, [2, 4]; 2, [1, 6; 3, 6]]; 2, [0, [1, 2]]; 4, [0, [1, 0]; 1, [2, 6]; 2, [3, 4]]|]
let startRule = 1

let acceptEmptyInput = false


let private lists_gotos = [|1; 3; 4; 2|]
let private small_gotos =
        [|3; 0; 131073; 262146; 65537; 262147; 262145; 262146|]
let gotos = Array.zeroCreate 5
for i = 0 to 4 do
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
let private lists_reduces = [|[|2,3|]; [|0,3|]|]
let private small_reduces =
        [|131073; 327680; 262145; 262145|]
let reduces = Array.zeroCreate 5
for i = 0 to 4 do
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
let private lists_zeroReduces = [|[|0|]|]
let private small_zeroReduces =
        [|1; 262144|]
let zeroReduces = Array.zeroCreate 5
for i = 0 to 4 do
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
let private small_acc = [3]
let private accStates = Array.zeroCreate 5
for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 5
let epsilonIndex = 6
let errorIndex = 1
let private parserSource = new ParserSourceReadBack<Token> (gotos, reduces, zeroReduces, accStates, nfas, leftSide, startRule, eofIndex, tokenToNumber, indexToSymbolType, acceptEmptyInput, numToString, epsilonIndex, errorIndex)
let buildAst : (seq<Token> -> ParseReadBackResult<Token>) =
    buildAstReadBack<Token> parserSource


