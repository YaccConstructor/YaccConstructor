module RNGLR.ParseExpr
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | EOF of 'a
        | N of 'a
        | P of 'a

let buildAst<'a> =
    let gotos =
        [|[|Some 1; None; None; None; Some 4; None|]
         ;[|None; None; None; None; None; Some 2|]
         ;[|Some 3; None; None; None; Some 4; None|]
         ;[|None; None; None; None; None; Some 2|]
         ;[|None; None; None; None; None; None|]
        |]
    let reduces =
        [|[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; [1,3]; []; [1,3]|]
         ;[|[]; []; []; [0,1]; []; [0,1]|]
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