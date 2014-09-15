
# 2 "Brackets_3.yrd.fs"
module RNGLR.ParseBrackets_3
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | CLOSE of (int)
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
    | OPEN x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "a"
    | 1 -> "error"
    | 2 -> "start"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "B"
    | 6 -> "C"
    | 7 -> "CLOSE"
    | 8 -> "OPEN"
    | 9 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | B _ -> 5
    | C _ -> 6
    | CLOSE _ -> 7
    | OPEN _ -> 8
    | RNGLR_EOF _ -> 9

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | CLOSE _ -> false
    | OPEN _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 3; 0; 0; 0; 0|]
let private rules = [|0; 2; 4; 5; 6; 8; 2; 7|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 8|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 3; 4; 5; 6; 7; 8|]
let private small_gotos =
        [|6; 0; 131073; 262146; 327683; 393220; 524293; 393222; 0; 131078; 262146; 327683; 393220; 524293; 458753; 458759|]
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
let private lists_reduces = [|[|0,1|]; [|2,1|]; [|3,1|]; [|4,1|]; [|5,3|]|]
let private small_reduces =
        [|65538; 458752; 589824; 196610; 458753; 589825; 262146; 458754; 589826; 327682; 458755; 589827; 524290; 458756; 589828|]
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
let private small_acc = [2]
let private accStates = Array.zeroCreate 9
for i = 0 to 8 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 9
let errorIndex = 1
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    let c = ref 0
    let seqToGraph = Seq.map (fun t -> let r = !c,[|t,!c+1|] in incr c;r) << Seq.takeWhile (fun t -> box t <> null)
    buildAstAbstract << seqToGraph


