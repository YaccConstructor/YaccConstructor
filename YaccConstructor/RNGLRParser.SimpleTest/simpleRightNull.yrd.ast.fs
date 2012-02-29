module RNGLR.ParseSimpleRightNull
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | EOF of 'a

let buildAst<'a> =
    let gotos =
        [|[|None; Some 1; None; None; Some 2; None|]
         ;[|None; None; None; None; None; None|]
         ;[|None; Some 3; None; None; Some 2; None|]
         ;[|None; None; Some 4; None; None; None|]
         ;[|None; None; None; None; None; None|]
        |]
    let reduces =
        [|[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; [1,1]|]
         ;[|[]; []; []; []; []; [1,2]|]
         ;[|[]; []; []; []; []; [1,3]|]
        |]
    let zeroReduces =
        [|[|[]; []; []; []; []; [2,0; 0,0]|]
         ;[|[]; []; []; []; []; []|]
         ;[|[]; []; []; []; []; [0,0]|]
         ;[|[]; []; []; []; []; [3,0]|]
         ;[|[]; []; []; []; []; []|]
        |]
    let accStates = [|true; true; false; false; false|]
    let rules =
        [|[||]
        ; [|4; 1; 2|]
        ; [|1|]
        ; [||]
        |]
    let leftSide =
        [|1; 1; 3; 2|]
    let startRule = 2
    let eofIndex = 5
    let tokenToNumber = function
        | A _ -> 4
        | EOF _ -> 5
    let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, leftSide, startRule, eofIndex, tokenToNumber)
    buildAst<_> parserSource