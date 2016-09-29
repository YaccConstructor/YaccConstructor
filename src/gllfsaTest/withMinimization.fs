module WithMinimization.r16s
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon

type Token =
    | A of unit

let tokenToNumber = function
    | A() -> 9

let stateToNontermName = function
    | 9 -> "A"
    | 0 -> "a"
    | 1 -> "s"
    | _ -> ""

let numIsTerminal = function
    | 9 -> true
    | _ -> false

let statesToConvert =
  [|[|1,5;9,6|];
    [|0,4|];
    [||];
    [|0,8|];
    [|0,3;9,3|];
    [|0,2|];
    [|0,2|];
    [|0,5|];
    [|0,7|] |]

let states =  
    statesToConvert 
    |> Array.Parallel.map (fun x ->  
        x 
        |> Array.map (fun (x,y) -> x, y * 1<state>)) 

let startState = 1 * 1<state>
let isFinalState = function
    | 2 -> true
    | 6 -> true
    | _ -> false

let nontermCount = 2

let firstSet =
  set[|0;
       65536|]

let private parserSource = new FSAParserSourceGLL (states, startState, isFinalState, nontermCount, numIsTerminal, stateToNontermName, firstSet)
let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource

