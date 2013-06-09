
# 2 "ComplexRightNull.yrd.fs"
module RNGLR.ParseComplexRightNull
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | A x -> box x
    | B x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "s"
    | 2 -> "t"
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
let leftSide = [|1; 1; 1; 3; 2|]
let private rules = [|4; 5; 2; 1; 1; 1|]
let private rulesStart = [|0; 0; 1; 5; 6; 6|]
let startRule = 3

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 4; 5; 7; 8; 6; 9; 10; 11|]
let private small_gotos =
        [|3; 65536; 262145; 327682; 196609; 131075; 262147; 65540; 262149; 327686; 327683; 65543; 262145; 327682; 524289; 131080; 589827; 65545; 262149; 327686; 655363; 65546; 262149; 327686|]
let gotos = Array.zeroCreate 12
for i = 0 to 11 do
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
let private lists_reduces = [|[|1,1|]; [|2,1|]; [|2,2|]; [|2,3|]; [|2,4|]|]
let private small_reduces =
        [|131073; 393216; 196609; 393217; 262145; 393218; 327681; 393219; 393217; 393220; 458755; 262144; 327680; 393216; 524291; 262145; 327681; 393217; 589827; 262146; 327682; 393218; 655363; 262147; 327683; 393219; 720899; 262148; 327684; 393220|]
let reduces = Array.zeroCreate 12
for i = 0 to 11 do
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
let private lists_zeroReduces = [|[|3; 0|]; [|4|]; [|0|]|]
let private small_zeroReduces =
        [|1; 393216; 196611; 262145; 327681; 393217; 262147; 262146; 327682; 393218; 327681; 393218; 524291; 262145; 327681; 393217; 589827; 262146; 327682; 393218; 655363; 262146; 327682; 393218|]
let zeroReduces = Array.zeroCreate 12
for i = 0 to 11 do
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
let private small_acc = [1; 0]
let private accStates = Array.zeroCreate 12
for i = 0 to 11 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 6
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource


