
# 2 "ManyReductions2.yrd.fs"
module RNGLR.ParseManyReductions2
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "a"
    | 1 -> "b"
    | 2 -> "c"
    | 3 -> "d"
    | 4 -> "error"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "B"
    | 8 -> "C"
    | 9 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | B _ -> 7
    | C _ -> 8
    | RNGLR_EOF _ -> 9

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|0; 5; 1; 2; 2; 3|]
let private rules = [|1; 2; 6; 0; 7; 3; 8; 4|]
let private rulesStart = [|0; 3; 4; 5; 6; 7; 8|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 8; 3; 5; 6; 7; 4|]
let private small_gotos =
        [|3; 0; 65537; 458754; 131076; 131075; 196612; 262149; 524294; 196609; 393223|]
let gotos = Array.zeroCreate 9
for i = 0 to 8 do
        gotos.[i] <- Array.zeroCreate 10
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
let private lists_reduces = [|[|0,3|]; [|3,1|]; [|5,1|]; [|4,1|]; [|2,1|]|]
let private small_reduces =
        [|262145; 589824; 327681; 393217; 393217; 393218; 458753; 393219; 524290; 262148; 524292|]
let reduces = Array.zeroCreate 9
for i = 0 to 8 do
        reduces.[i] <- Array.zeroCreate 10
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
let zeroReduces = Array.zeroCreate 9
for i = 0 to 8 do
        zeroReduces.[i] <- Array.zeroCreate 10
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
let private accStates = Array.zeroCreate 9
for i = 0 to 8 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 9
let errorIndex = 4
let errorRulesExists = true
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


