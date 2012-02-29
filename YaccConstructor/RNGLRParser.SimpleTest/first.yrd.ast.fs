module RNGLR.ParseFirst
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | B of 'a
        | EOF of 'a

let buildAst<'a> =
    let small_gotos =
        [|[|0,1; 3,2; 4,4|]
         ;[||]
         ;[|0,3; 3,2; 4,4|]
         ;[||]
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
         ;[|5,[0,2]|]
         ;[|5,[1,1]|]
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
        [|[|3; 0|]
        ; [|4|]
        ; [|0|]
        |]
    let leftSide =
        [|0; 0; 2|]
    let startRule = 2
    let eofIndex = 5
    let tokenToNumber = function
        | A _ -> 3
        | B _ -> 4
        | EOF _ -> 5
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource