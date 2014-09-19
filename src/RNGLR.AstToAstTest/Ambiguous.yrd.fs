
# 2 "Ambiguous.yrd.fs"
module RNGLR.ParseAmbiguous
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
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
    | 0 -> "error"
    | 1 -> "highlight_A"
    | 2 -> "start"
    | 3 -> "yard_start_rule"
    | 4 -> "A"
    | 5 -> "RNGLR_EOF"
    | _ -> ""

let tokenToNumber = function
    | A _ -> 4
    | RNGLR_EOF _ -> 5

let isLiteral = function
    | A _ -> false
    | RNGLR_EOF _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|2; 2; 3; 1|]
let private rules = [|2; 2; 1; 2; 4|]
let private rulesStart = [|0; 2; 3; 4; 5|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 4; 3|]
let private small_gotos =
        [|3; 65536; 131073; 262146; 131075; 65536; 131075; 262146; 196611; 65536; 131075; 262146|]
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
let private lists_reduces = [|[|1,1|]; [|0,2|]; [|3,1|]|]
let private small_reduces =
        [|65538; 262144; 327680; 196610; 262145; 327681; 262146; 262146; 327682|]
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
let private lists_zeroReduces = [||]
let private small_zeroReduces =
        [||]
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
let private small_acc = [2]
let private accStates = Array.zeroCreate 5
for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 5
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    let c = ref 0
    let seqToGraph = Seq.map (fun t -> let r = !c,[|t,!c+1|] in incr c;r) << Seq.takeWhile (fun t -> box t <> null)
    buildAstAbstract << seqToGraph


