module TryGenerator.program

open Lexer
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open parser

let compCalc = ws >>. file  .>> eof

let gogo s = run compCalc s

let path = "..\..\Calc.Fparsec\calc.yrd"
let text = System.IO.File.ReadAllText path

let checkInPut =
    function
    | Success (v, _, _)  -> printfn "%s" (v.ToString())
    | Failure (msg, err, _) -> printf "%s" msg; failwith msg


let test4() = (gogo text) |> checkInPut 
do test4()

