module RNGLR.ParseComplexRightNull
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
type Token =
    | A of int
    | B of int
    | EOF of int

let numToString = function 
    | 0 -> "s"
    | 1 -> "t"
    | 2 -> "yard_start_rule"
    | 3 -> "A"
    | 4 -> "B"
    | 5 -> "EOF"
    | _ -> ""
let tokenToNumber = function
    | A _ -> 3
    | B _ -> 4
    | EOF _ -> 5

let leftSide = [|0; 0; 0; 2; 1|]
let private rules = [|3; 4; 1; 0; 0; 0|]
let private rulesStart = [|0; 0; 1; 5; 6; 6|]
let startRule = 3

let defaultAstToDot = 
    let getRight prod = seq {for i = rulesStart.[prod] to rulesStart.[prod+1]-1 do yield rules.[i]}
    let startInd = leftSide.[startRule]
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot startInd numToString getRight)

let inline unpack x = x >>> 16, x <<< 16 >>> 16
let private small_gotos =
        [|0, [|0,1; 3,2; 4,3|]; 3, [|1,4|]; 4, [|0,5; 3,2; 4,3|]; 5, [|0,6; 3,2; 4,3|]|]
let private gotos = Array.zeroCreate 7
for i = 0 to 6 do
        gotos.[i] <- Array.create 6 None
for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
let private lists_reduces = [|[||]; [|1,1|]; [|2,1|]; [|2,2|]; [|2,3|]; [|2,4|]|]
let private small_reduces =
        [|131075; 196609; 262145; 327681; 196611; 196610; 262146; 327682; 262147; 196611; 262147; 327683; 327683; 196612; 262148; 327684; 393219; 196613; 262149; 327685|]
let reduces = Array.zeroCreate 7
for i = 0 to 6 do
        reduces.[i] <- Array.create 6 [||]
let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
let private lists_zeroReduces = [|[||]; [|3; 0|]; [|4|]; [|0|]|]
let private small_zeroReduces =
        [|1; 327681; 196611; 196610; 262146; 327682; 262147; 196611; 262147; 327683; 327683; 196611; 262147; 327683|]
let zeroReduces = Array.zeroCreate 7
for i = 0 to 6 do
        zeroReduces.[i] <- Array.create 6 [||]
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
let private accStates = Array.zeroCreate 7
for i = 0 to 6 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 5
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

