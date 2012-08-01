module RNGLR.ParseFirst
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function 
    | 0 -> "a"
    | 1 -> "yard_start_rule"
    | 2 -> "A"
    | 3 -> "B"
    | 4 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 2
    | B _ -> 3
    | EOF _ -> 4

let leftSide = [|0; 0; 1|]
let private rules = [|2; 0; 3; 0|]
let private rulesStart = [|0; 2; 3; 4|]
let startRule = 2

let acceptEmptyInput = false

let defaultAstToDot = 
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let inline unpack x = x >>> 16, x <<< 16 >>> 16
let private small_gotos =
        [|0, [|0,1; 2,2; 3,4|]; 2, [|0,3; 2,2; 3,4|]|]
let private gotos = Array.zeroCreate 5
for i = 0 to 4 do
        gotos.[i] <- Array.create 5 None
for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
let private lists_reduces = [|[||]; [|0,2|]; [|1,1|]|]
let private small_reduces =
        [|196609; 262145; 262145; 262146|]
let reduces = Array.zeroCreate 5
for i = 0 to 4 do
        reduces.[i] <- Array.create 5 [||]
let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
let private lists_zeroReduces = [|[||]|]
let private small_zeroReduces =
        [||]
let zeroReduces = Array.zeroCreate 5
for i = 0 to 4 do
        zeroReduces.[i] <- Array.create 5 [||]
let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
let private small_acc = [1]
let private accStates = Array.zeroCreate 5
for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 4
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

