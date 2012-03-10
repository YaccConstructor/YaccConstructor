module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
        | EOF of int
        | N of int
        | P of int

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 4,4|]; 1, [|5,2|]; 2, [|0,3; 4,4|]; 3, [|5,2|]|]
    let gotos = Array.zeroCreate 5
    for i = 0 to 4 do
        gotos.[i] <- Array.create 6 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [1,3]; [0,1]|]
    let small_reduces =
        [|196610; 196609; 327681; 262146; 196610; 327682|]
    let reduces = Array.zeroCreate 5
    for i = 0 to 4 do
        reduces.[i] <- Array.create 6 []
    let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
    let lists_zeroReduces = [|[]|]
    let small_zeroReduces =
        [||]
    let zeroReduces = Array.zeroCreate 5
    for i = 0 to 4 do
        zeroReduces.[i] <- Array.create 6 []
    let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
    let small_acc = [1]
    let accStates = Array.zeroCreate 5
    for i = 0 to 4 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let rules = [|4; 0; 5; 0; 0|]
    let rulesStart = [|0; 1; 4|]
    let leftSide =
        [|0; 0; 2|]
    let startRule = 2
    let eofIndex = 3
    let tokenToNumber = function
        | EOF _ -> 3
        | N _ -> 4
        | P _ -> 5
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource