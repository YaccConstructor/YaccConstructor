module RNGLR.ParseExpr
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | EOF of int
    | N of int
    | P of int

let numToString = function 
    | 0 -> "e"
    | 1 -> "yard_start_rule"
    | 2 -> "EOF"
    | 3 -> "N"
    | 4 -> "P"
    | _ -> ""
let tokenToNumber = function
    | EOF _ -> 2
    | N _ -> 3
    | P _ -> 4

let leftSide = [|0; 0; 1|]
let private rules = [|3; 0; 4; 0; 0|]
let private rulesStart = [|0; 1; 4; 5|]
let startRule = 2

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot startInd numToString getRight)

let inline unpack x = x >>> 16, x <<< 16 >>> 16
let private small_gotos =
        [|0, [|0,1; 3,4|]; 1, [|4,2|]; 2, [|0,3; 3,4|]; 3, [|4,2|]|]
let private gotos = Array.zeroCreate 5
for i = 0 to 4 do
        gotos.[i] <- Array.create 5 None
for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
let private lists_reduces = [|[||]; [|1,3|]; [|0,1|]|]
let private small_reduces =
        [|196610; 131073; 262145; 262146; 131074; 262146|]
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
let eofIndex = 2
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

