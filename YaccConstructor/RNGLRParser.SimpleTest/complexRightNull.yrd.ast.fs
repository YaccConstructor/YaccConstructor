module RNGLR.ParseComplexRightNull
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | B of 'a
        | EOF of 'a

let buildAst<'a> =
    let gotos =
        [|[|None; Some 1; None; None; Some 2; Some 3; None|]
         ;[|None; None; None; None; None; None; None|]
         ;[|None; None; None; None; None; None; None|]
         ;[|None; None; Some 4; None; None; None; None|]
         ;[|None; Some 5; None; None; Some 2; Some 3; None|]
         ;[|None; Some 6; None; None; Some 2; Some 3; None|]
         ;[|None; None; None; None; None; None; None|]
        |]
    let reduces =
        [|[|[]; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []|]
         ;[|[]; []; []; []; [1,1]; [1,1]; [1,1]|]
         ;[|[]; []; []; []; [2,1]; [2,1]; [2,1]|]
         ;[|[]; []; []; []; [2,2]; [2,2]; [2,2]|]
         ;[|[]; []; []; []; [2,3]; [2,3]; [2,3]|]
         ;[|[]; []; []; []; [2,4]; [2,4]; [2,4]|]
        |]
    let zeroReduces =
        [|[|[]; []; []; []; []; []; [3,0; 0,0]|]
         ;[|[]; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []|]
         ;[|[]; []; []; []; [4,0]; [4,0]; [4,0]|]
         ;[|[]; []; []; []; [0,0]; [0,0]; [0,0]|]
         ;[|[]; []; []; []; [0,0]; [0,0]; [0,0]|]
         ;[|[]; []; []; []; []; []; []|]
        |]
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