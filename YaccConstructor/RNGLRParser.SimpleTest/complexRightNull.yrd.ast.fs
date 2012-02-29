module RNGLR.ParseComplexRightNull
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | B of 'a
        | EOF of 'a

let buildAst<'a> =
    let small_gotos =
        [|[|1,1; 4,2; 5,3|]
         ;[||]
         ;[||]
         ;[|2,4|]
         ;[|1,5; 4,2; 5,3|]
         ;[|1,6; 4,2; 5,3|]
         ;[||]
        |]
    let gotos = Array.zeroCreate 7
    for i = 0 to 6 do
        gotos.[i] <- Array.create 7 None
        for (x,y) in small_gotos.[i] do
            gotos.[i].[x] <- Some  y
    let small_reduces =
        [|[||]
         ;[||]
         ;[|4,[1,1]; 5,[1,1]; 6,[1,1]|]
         ;[|4,[2,1]; 5,[2,1]; 6,[2,1]|]
         ;[|4,[2,2]; 5,[2,2]; 6,[2,2]|]
         ;[|4,[2,3]; 5,[2,3]; 6,[2,3]|]
         ;[|4,[2,4]; 5,[2,4]; 6,[2,4]|]
        |]
    let reduces = Array.zeroCreate 7
    for i = 0 to 6 do
        reduces.[i] <- Array.create 7 []
        for (x,y) in small_reduces.[i] do
            reduces.[i].[x] <-  y
    let small_zeroReduces =
        [|[|6,[3,0; 0,0]|]
         ;[||]
         ;[||]
         ;[|4,[4,0]; 5,[4,0]; 6,[4,0]|]
         ;[|4,[0,0]; 5,[0,0]; 6,[0,0]|]
         ;[|4,[0,0]; 5,[0,0]; 6,[0,0]|]
         ;[||]
        |]
    let zeroReduces = Array.zeroCreate 7
    for i = 0 to 6 do
        zeroReduces.[i] <- Array.create 7 []
        for (x,y) in small_zeroReduces.[i] do
            zeroReduces.[i].[x] <-  y
    let accStates = [|true; true; false; false; false; false; false|]
    let rules =
        [|[||]
        ; [|4|]
        ; [|5; 2; 1; 1|]
        ; [|1|]
        ; [||]
        |]
    let leftSide =
        [|1; 1; 1; 3; 2|]
    let startRule = 3
    let eofIndex = 6
    let tokenToNumber = function
        | A _ -> 4
        | B _ -> 5
        | EOF _ -> 6
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource