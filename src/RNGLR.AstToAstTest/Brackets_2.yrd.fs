
# 2 "Brackets_2.yrd.fs"
module RNGLR.ParseBrackets_2
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | CLOSE of (int)
    | D of (int)
    | OPEN of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | CLOSE x -> box x
    | D x -> box x
    | OPEN x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "a"
    | 1 -> "error"
    | 2 -> "left"
    | 3 -> "right"
    | 4 -> "start"
    | 5 -> "yard_start_rule"
    | 6 -> "A"
    | 7 -> "B"
    | 8 -> "C"
    | 9 -> "CLOSE"
    | 10 -> "D"
    | 11 -> "OPEN"
    | 12 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 6
    | B _ -> 7
    | C _ -> 8
    | CLOSE _ -> 9
    | D _ -> 10
    | OPEN _ -> 11
    | RNGLR_EOF _ -> 12

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | CLOSE _ -> false
    | D _ -> false
    | OPEN _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|4; 5; 0; 2; 3|]
let private rules = [|2; 0; 3; 4; 6; 7; 8; 10; 11; 9|]
let private rulesStart = [|0; 3; 4; 8; 9; 10|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 9; 10; 2; 5; 3; 4; 6; 7; 8|]
let private small_gotos =
        [|3; 131072; 262145; 720898; 65538; 3; 393220; 131074; 196613; 589830; 327681; 458759; 393217; 524296; 458753; 655369|]
let gotos = Array.zeroCreate 11
for i = 0 to 10 do
        gotos.[i] <- Array.zeroCreate 13
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
let private lists_reduces = [|[|0,3|]; [|4,1|]; [|2,4|]; [|3,1|]|]
let private small_reduces =
        [|196609; 786432; 262145; 786433; 524289; 589826; 655361; 393219|]
let reduces = Array.zeroCreate 11
for i = 0 to 10 do
        reduces.[i] <- Array.zeroCreate 13
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
let zeroReduces = Array.zeroCreate 11
for i = 0 to 10 do
        zeroReduces.[i] <- Array.zeroCreate 13
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
let private small_acc = [9]
let private accStates = Array.zeroCreate 11
for i = 0 to 10 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 12
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    let c = ref 0
    let seqToGraph = Seq.map (fun t -> let r = !c,[|t,!c+1|] in incr c;r) << Seq.takeWhile (fun t -> box t <> null)
    buildAstAbstract << seqToGraph


