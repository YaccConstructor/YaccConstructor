// Learn more about F# at http://fsharp.net


module TryGenerator.program

open Lexer
open test
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

let compCalc = ws >>. expr  .>> eof



let gogo s = run compCalc s



let checkInPut =
    function
    | Success (v, _, _)  -> printfn "%s" (v.ToString())
    | Failure (msg, err, _) -> printf "%s" msg; failwith msg



let test4() = (gogo  "  a := 3 + (1 * 4)
                        b := 5**1
                        a + b   " ) |> checkInPut 

//let test2() = (gogo " 3 void 4 ") |> checkInPut
do test4()