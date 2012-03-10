module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token =
        | A of int
        | B of int
        | EOF of int

let buildAst<'a> =
    let inline unpack x = x >>> 16, x <<< 16 >>> 16
    let small_gotos =
        [|0, [|1,1; 4,2; 5,3|]; 3, [|2,4|]; 4, [|1,5; 4,2; 5,3|]; 5, [|1,6; 4,2; 5,3|]|]
    let gotos = Array.zeroCreate 7
    for i = 0 to 6 do
        gotos.[i] <- Array.create 7 None
    for (i,t) in small_gotos do
        for (j,x) in t do
            gotos.[i].[j] <- Some  x
    let lists_reduces = [|[]; [1,1]; [2,1]; [2,2]; [2,3]; [2,4]|]
    let small_reduces =
        [|131075; 262145; 327681; 393217; 196611; 262146; 327682; 393218; 262147; 262147; 327683; 393219; 327683; 262148; 327684; 393220; 393219; 262149; 327685; 393221|]
    let reduces = Array.zeroCreate 7
    for i = 0 to 6 do
        reduces.[i] <- Array.create 7 []
    let init_reduces =
        let mutable cur = 0
        while cur < small_reduces.Length do
            let i,length = unpack small_reduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_reduces.[cur + k]
                reduces.[i].[j] <-  lists_reduces.[x]
            cur <- cur + length
    let lists_zeroReduces = [|[]; [3; 0]; [4]; [0]|]
    let small_zeroReduces =
        [|1; 393217; 196611; 262146; 327682; 393218; 262147; 262147; 327683; 393219; 327683; 262147; 327683; 393219|]
    let zeroReduces = Array.zeroCreate 7
    for i = 0 to 6 do
        zeroReduces.[i] <- Array.create 7 []
    let init_zeroReduces =
        let mutable cur = 0
        while cur < small_zeroReduces.Length do
            let i,length = unpack small_zeroReduces.[cur]
            cur <- cur + 1
            for k = 0 to length-1 do
                let j,x = unpack small_zeroReduces.[cur + k]
                zeroReduces.[i].[j] <-  lists_zeroReduces.[x]
            cur <- cur + length
    let small_acc = [1; 0]
    let accStates = Array.zeroCreate 7
    for i = 0 to 6 do
        accStates.[i] <- List.exists ((=) i) small_acc
    let rules = [|4; 5; 2; 1; 1; 1|]
    let rulesStart = [|0; 0; 1; 5; 6|]
    let leftSide =
        [|1; 1; 1; 3; 2|]
    let startRule = 3
    let eofIndex = 6
    let tokenToNumber = function
        | A _ -> 4
        | B _ -> 5
        | EOF _ -> 6
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource