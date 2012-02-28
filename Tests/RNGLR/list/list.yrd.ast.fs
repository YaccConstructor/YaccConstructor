module RNGLR.Parse
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
type Token<'a> =
        | A of 'a
        | B of 'a
        | C of 'a
        | EOF of 'a

let buildAst<'a> =
    let gotos =
        [|[|Some 1; None; Some 2; None; Some 5; Some 6; None; None|]
         ;[|None; None; None; None; None; None; None; None|]
         ;[|None; None; None; None; None; None; Some 3; None|]
         ;[|Some 4; None; None; None; Some 5; Some 6; None; None|]
         ;[|None; None; None; None; None; None; None; None|]
         ;[|None; None; None; None; None; None; None; None|]
         ;[|None; None; None; None; None; None; None; None|]
        |]
    let reduces =
        [|[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; [0,1]; [0,1]|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; [1,3]; [1,3]|]
         ;[|[]; []; []; []; []; []; [4,1]; [4,1]|]
         ;[|[]; []; []; []; []; []; [3,1]; [3,1]|]
        |]
    let zeroReduces =
        [|[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
         ;[|[]; []; []; []; []; []; []; []|]
        |]
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