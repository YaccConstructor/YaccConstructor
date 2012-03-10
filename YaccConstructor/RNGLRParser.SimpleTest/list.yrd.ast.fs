module RNGLR.ParseList
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
        | A of int
        | B of int
        | C of int
        | EOF of int

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|0,1; 2,2; 4,5; 5,6|]; 2, [|6,3|]; 3, [|0,4; 4,5; 5,6|]|]
    let gotos = Array.zeroCreate 7
    for i = 0 to 6 do
        gotos.[i] <- Array.create 8 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [0,1]; [1,3]; [4,1]; [3,1]|]
    let small_reduces =
        [|65538; 393217; 458753; 262146; 393218; 458754; 327682; 393219; 458755; 393218; 393220; 458756|]
    let reduces = Array.zeroCreate 7
    for i = 0 to 6 do
        reduces.[i] <- Array.create 8 []
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
    let zeroReduces = Array.zeroCreate 7
    for i = 0 to 6 do
        zeroReduces.[i] <- Array.create 8 []
    let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
    let small_acc = [2]
    let accStates = Array.zeroCreate 7
    for i = 0 to 6 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let rules = [|0; 2; 6; 0; 2; 5; 4|]
    let rulesStart = [|0; 1; 4; 5; 6|]
    let leftSide =
        [|2; 2; 3; 0; 0|]
    let startRule = 2
    let eofIndex = 7
    let tokenToNumber = function
        | A _ -> 4
        | B _ -> 5
        | C _ -> 6
        | EOF _ -> 7
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource