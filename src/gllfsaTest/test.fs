module GLLFSA.test
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon

type Token =
    | A of unit

let tokenToNumber = function
    | A() -> 5

let stateToNontermName = function
    | 5 -> "A"
    | 0 -> "full"
    | _ -> ""

let private numOfAnyState = -1<state>

let private numIsTerminal = function
    | 5 -> true
    | _ -> false

let private stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<state>>()
stateAndTokenToNewState.Add(0, 4<state>)
stateAndTokenToNewState.Add(131072, 1<state>)
stateAndTokenToNewState.Add(196608, 2<state>)
stateAndTokenToNewState.Add(262144, 3<state>)

let private outNonterms =
  [|[||];
    [||];
    [||];
    [||];
    [||] |]

let private startState = 0<state>

let private finalStates =
  new System.Collections.Generic.HashSet<int<state>>(
     [|1<state>|])

let private nontermCount = 1

let private parserSource = new FSAParserSourceGLL (outNonterms, startState, finalStates, nontermCount, numIsTerminal, stateToNontermName, numOfAnyState, stateAndTokenToNewState)

let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource


