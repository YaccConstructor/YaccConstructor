module GLL.longK
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common

type Token =
    | A of (int)

let stringToToken = new System.Collections.Generic.Dictionary<_,_>()
stringToToken.Add("A",9<token>)

let intToString = new System.Collections.Generic.Dictionary<_,_>()
intToString.Add(9,"A")
intToString.Add(0,"k")
intToString.Add(1,"s")

let private anyNonterm = -1<positionInGrammar>

let private terminalNums = new System.Collections.Generic.HashSet<_>()
terminalNums.Add(9<token>) |> ignore

let private stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<positionInGrammar>[]>()
stateAndTokenToNewState.Add(0, [|6<positionInGrammar>|])
stateAndTokenToNewState.Add(262144, [|3<positionInGrammar>|])

let private outNonterms =
  [|[|1<positionInGrammar>,5<positionInGrammar>|];
    [|0<positionInGrammar>,4<positionInGrammar>|];
    [||];
    [|0<positionInGrammar>,8<positionInGrammar>|];
    [|0<positionInGrammar>,3<positionInGrammar>|];
    [|0<positionInGrammar>,2<positionInGrammar>|];
    [|0<positionInGrammar>,2<positionInGrammar>|];
    [|0<positionInGrammar>,5<positionInGrammar>|];
    [|0<positionInGrammar>,7<positionInGrammar>|] |]

let private startState = 1<positionInGrammar>

let private finalStates =
  new System.Collections.Generic.HashSet<int<positionInGrammar>>(
     [|6<positionInGrammar>;
       2<positionInGrammar>|])

let private nontermCount = 2

let private multipleInEdges = 
    [|false; false; true; true; false; true; false; false; false|]

let parserSource = new ParserSourceGLL (outNonterms, startState, finalStates, nontermCount, terminalNums, intToString, anyNonterm, stateAndTokenToNewState,stringToToken, multipleInEdges)


