module WithoutMinimization.r16s
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon

type Token =
    | A of unit

let tokenToNumber = function
    | A() -> 15

let stateToNontermName = function
    | 15 -> "A"
    | 0 -> "s"
    | 1 -> "a"
    | _ -> ""

let numIsTerminal = function
    | 15 -> true
    | _ -> false

let statesToConvert =
  [|[|1,3|];
    [|0,13;15,14|];
    [||];
    [|1,4;15,8|];
    [|1,5|];
    [|1,6|];
    [|1,7|];
    [|1,2|];
    [|1,9|];
    [|1,10|];
    [|1,11|];
    [|1,2|];
    [||];
    [|1,12|];
    [|1,12|] |]

let states =  
    statesToConvert 
    |> Array.Parallel.map (fun x ->  
        x 
        |> Array.map (fun (x,y) -> x, y * 1<state>)) 

let startState = 0 * 1<state>
let isFinalState = function
    | 2 -> true
    | 14 -> true
    | 12 -> true
    | _ -> false

let nontermCount = 2

let firstSet =
  set[|65536;
       0|]

let private parserSource = new FSAParserSourceGLL (states, startState, isFinalState, nontermCount, numIsTerminal, stateToNontermName, firstSet)
let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource

