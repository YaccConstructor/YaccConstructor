module RNGLR.ParseExpr
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | EOF of 'a
        | N of 'a
        | P of 'a

let buildAst<'a> =
    let small_gotos =
        [|[|0,1; 4,4|]
         ;[|5,2|]
         ;[|0,3; 4,4|]
         ;[|5,2|]
         ;[||]
        |]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 6 None
        for (x,y) in small_gotos.[i] do
            gotos.[i].[x] <- Some  y
    let small_reduces =
        [|[||]
         ;[||]
         ;[||]
         ;[|3,[1,3]; 5,[1,3]|]
         ;[|3,[0,1]; 5,[0,1]|]
        |]
    let reduces = Array.zeroCreate 5
    for i = 0 to 4 do
        reduces.[i] <- Array.create 6 []
        for (x,y) in small_reduces.[i] do
            reduces.[i].[x] <-  y
    let small_zeroReduces =
        [|[||]
         ;[||]
         ;[||]
         ;[||]
         ;[||]
        |]
    let zeroReduces = Array.zeroCreate 5
    for i = 0 to 4 do
        zeroReduces.[i] <- Array.create 6 []
        for (x,y) in small_zeroReduces.[i] do
            zeroReduces.[i].[x] <-  y
    let accStates = [|false; true; false; false; false|]
    let rules =
        [|[|4|]
        ; [|0; 5; 0|]
        ; [|0|]
        |]
    let leftSide =
        [|0; 0; 2|]
    let startRule = 2
    let eofIndex = 3
    let tokenToNumber = function
        | EOF _ -> 3
        | N _ -> 4
        | P _ -> 5
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource