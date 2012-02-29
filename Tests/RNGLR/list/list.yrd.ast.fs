module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | B of 'a
        | C of 'a
        | EOF of 'a

let buildAst<'a> =
    let small_gotos =
        [|[|0,1; 2,2; 4,5; 5,6|]
         ;[||]
         ;[|6,3|]
         ;[|0,4; 4,5; 5,6|]
         ;[||]
         ;[||]
         ;[||]
        |]
    let gotos = Array.zeroCreate 7
    for i = 0 to 6 do
        gotos.[i] <- Array.create 8 None
        for (x,y) in small_gotos.[i] do
            gotos.[i].[x] <- Some  y
    let small_reduces =
        [|[||]
         ;[|6,[0,1]; 7,[0,1]|]
         ;[||]
         ;[||]
         ;[|6,[1,3]; 7,[1,3]|]
         ;[|6,[4,1]; 7,[4,1]|]
         ;[|6,[3,1]; 7,[3,1]|]
        |]
    let reduces = Array.zeroCreate 7
    for i = 0 to 6 do
        reduces.[i] <- Array.create 8 []
        for (x,y) in small_reduces.[i] do
            reduces.[i].[x] <-  y
    let small_zeroReduces =
        [|[||]
         ;[||]
         ;[||]
         ;[||]
         ;[||]
         ;[||]
         ;[||]
        |]
    let zeroReduces = Array.zeroCreate 7
    for i = 0 to 6 do
        zeroReduces.[i] <- Array.create 8 []
        for (x,y) in small_zeroReduces.[i] do
            zeroReduces.[i].[x] <-  y
    let accStates = [|false; false; true; false; false; false; false|]
    let rules =
        [|[|0|]
        ; [|2; 6; 0|]
        ; [|2|]
        ; [|5|]
        ; [|4|]
        |]
    let leftSide =
        [|2; 2; 3; 0; 0|]
    let startRule = 2
    let eofIndex = 7
    let tokenToNumber = function
        | A _ -> 4
        | B _ -> 5
        | C _ -> 6
        | EOF _ -> 7
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource