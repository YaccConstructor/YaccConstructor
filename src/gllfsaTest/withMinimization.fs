module WithMinimization.r16s
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon

type Token =
    | A of unit
    | B of unit
    | C of unit
    | D of unit
    | E of unit
    | F of unit
    | L of unit
    | K of unit

let tokenToNumber = function
    | A() -> 76
    | B() -> 77
    | C() -> 78
    | D() -> 79
    | E() -> 80
    | F() -> 81
    | L() -> 82
    | K() -> 83

let numToString = function
    | 76 -> "A"
    | 77 -> "B"
    | 78 -> "C"
    | 79 -> "D"
    | 80 -> "E"
    | 81 -> "F"
    | 82 -> "L"
    | 83 -> "K"
    | 0 -> "s"
    | 1 -> "a"
    | 2 -> "b"
    | 3 -> "c"
    | 4 -> "d"
    | 5 -> "e"
    | 6 -> "f"
    | 7 -> "l"
    | 8 -> "k"
    | _ -> ""

let numIsTerminal = function
    | 76 -> true
    | 77 -> true
    | 78 -> true
    | 79 -> true
    | 80 -> true
    | 81 -> true
    | 82 -> true
    | 83 -> true
    | _ -> false

let numIsEpsilon = function
    | -1 -> true
    | _ -> false

let statesToConvert =
  [|[|1,12;2,12;3,12;4,12;5,12;6,12|];
    [|76,13|];
    [|76,9;77,9|];
    [|76,9;78,9|];
    [|76,9;79,9|];
    [|76,9;80,9|];
    [|76,9;81,9|];
    [|82,9|];
    [|83,9|];
    [||];
    [|8,75|];
    [|0,9|];
    [|7,10|];
    [|76,13|];
    [|8,11|];
    [|7,14|];
    [|8,15|];
    [|7,16|];
    [|8,17|];
    [|7,18|];
    [|8,19|];
    [|7,20|];
    [|8,21|];
    [|7,22|];
    [|8,23|];
    [|7,24|];
    [|8,25|];
    [|7,26|];
    [|8,27|];
    [|7,28|];
    [|8,29|];
    [|7,30|];
    [|8,31|];
    [|7,32|];
    [|8,33|];
    [|7,34|];
    [|8,35|];
    [|7,36|];
    [|8,37|];
    [|7,38|];
    [|8,39|];
    [|7,40|];
    [|8,41|];
    [|7,42|];
    [|8,43|];
    [|7,44|];
    [|8,45|];
    [|7,46|];
    [|8,47|];
    [|7,48|];
    [|8,49|];
    [|7,50|];
    [|8,51|];
    [|7,52|];
    [|8,53|];
    [|7,54|];
    [|8,55|];
    [|7,56|];
    [|8,57|];
    [|7,58|];
    [|8,59|];
    [|7,60|];
    [|8,61|];
    [|7,62|];
    [|8,63|];
    [|7,64|];
    [|8,65|];
    [|7,66|];
    [|8,67|];
    [|7,68|];
    [|8,69|];
    [|7,70|];
    [|8,71|];
    [|7,72|];
    [|8,73|];
    [|7,74|] |]

let states =  
    statesToConvert 
    |> Array.Parallel.map (fun x ->  
        x 
        |> Array.map (fun (x,y) -> x, y * 1<state>)) 
let startState = 0 * 1<state>
let finalState = 9 * 1<state>
let nontermCount = 9

let firstSet =
  set[|65536;65537;65538;65539;65540;65541;
       0;1;2;3;4;5;
       131072;131073;
       196608;196610;
       262144;262147;
       327680;327684;
       393216;393221;
       458758;
       524295|]

let private parserSource = new FSAParserSourceGLL (states, startState, finalState, nontermCount, numIsTerminal, numIsEpsilon, numToString, firstSet)
let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource

