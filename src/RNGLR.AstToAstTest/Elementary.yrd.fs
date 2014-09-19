
# 2 "Elementary.yrd.fs"
module RNGLR.ParseElementary
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of (int)
    | B of (int)
    | C of (int)
    | D of (int)
    | RNGLR_EOF of (int)

let genLiteral (str : string) (data : int) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | A x -> box x
    | B x -> box x
    | C x -> box x
    | D x -> box x
    | RNGLR_EOF x -> box x

let numToString = function
    | 0 -> "epsilon"
    | 1 -> "error"
    | 2 -> "highlight_A"
    | 3 -> "highlight_B"
    | 4 -> "highlight_C"
    | 5 -> "highlight_D"
    | 6 -> "start"
    | 7 -> "yard_start_rule"
    | 8 -> "A"
    | 9 -> "B"
    | 10 -> "C"
    | 11 -> "D"
    | 12 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 8
    | B _ -> 9
    | C _ -> 10
    | D _ -> 11
    | RNGLR_EOF _ -> 12

let isLiteral = function
    | A _ -> false
    | B _ -> false
    | C _ -> false
    | D _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|6; 6; 7; 0; 2; 3; 4; 5|]
let private rules = [|2; 3; 4; 5; 2; 0; 4; 6; 8; 9; 10; 11|]
let private rulesStart = [|0; 4; 7; 8; 8; 9; 10; 11; 12|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 10; 11; 2; 5; 9; 3; 4; 6; 7; 8|]
let private small_gotos =
        [|3; 131072; 393217; 524290; 65539; 3; 196612; 589829; 131074; 262150; 655367; 327682; 262152; 655367; 393218; 327689; 720906|]
let gotos = Array.zeroCreate 12
for i = 0 to 11 do
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
let private lists_reduces = [|[|1,3|]; [|6,1|]; [|0,4|]; [|7,1|]; [|5,1|]; [|4,1|]|]
let private small_reduces =
        [|196609; 786432; 262146; 720897; 786433; 458753; 786434; 524289; 786435; 589825; 655364; 720898; 589829; 655365|]
let reduces = Array.zeroCreate 12
for i = 0 to 11 do
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
let private lists_zeroReduces = [|[|3|]|]
let private small_zeroReduces =
        [|65537; 655360|]
let zeroReduces = Array.zeroCreate 12
for i = 0 to 11 do
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
let private small_acc = [10]
let private accStates = Array.zeroCreate 12
for i = 0 to 11 do
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


