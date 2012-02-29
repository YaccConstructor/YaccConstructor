module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | B of 'a
        | EOF of 'a

let buildAst<'a> =
    let gotos =
        [|[|Some 1; None; None; Some 2; Some 4; None|]
         ;[|None; None; None; None; None; None|]
         ;[|Some 3; None; None; Some 2; Some 4; None|]
         ;[|None; None; None; None; None; None|]
         ;[|None; None; None; None; None; None|]
        |]
    let reduces =
        [|[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; [0,2]|]
         ;[|[]; []; []; []; []; [1,1]|]
        |]
    let zeroReduces =
        [|[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
        |]
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