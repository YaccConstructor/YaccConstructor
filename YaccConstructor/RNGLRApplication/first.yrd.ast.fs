module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
        | A of int
        | B of int
        | EOF of int

let buildAst<'a> =
    let small_gotos =
        [|0, [|0,1; 3,2; 4,4|]; 2, [|0,3; 3,2; 4,4|]|]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 6 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [0,2]; [1,1]|]
    let small_reduces =
        [|3, [|5,1|]; 4, [|5,2|]|]
    let reduces = Array.zeroCreate 5
    for i = 0 to 4 do
        reduces.[i] <- Array.create 6 []
    for (i,t) in small_reduces do
        for (j,x) in t do
            reduces.[i].[j] <-  lists_reduces.[x]
    let lists_zeroReduces = [|[]|]
    let small_zeroReduces =
        [||]
    let zeroReduces = Array.zeroCreate 5
    for i = 0 to 4 do
        zeroReduces.[i] <- Array.create 6 []
    for (i,t) in small_zeroReduces do
        for (j,x) in t do
            zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
    let accStates = [|false; true; false; false; false|]
    let rules = [|3; 0; 4; 0|]
    let rulesStart = [|0; 2; 3|]
    let leftSide =
        [|0; 0; 2|]
    let startRule = 2
    let eofIndex = 5
    let tokenToNumber = function
        | A _ -> 3
        | B _ -> 4
        | EOF _ -> 5
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource