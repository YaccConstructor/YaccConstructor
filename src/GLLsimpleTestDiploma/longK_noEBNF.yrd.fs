module GLL.longK_noEBNF
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common

type Token =
    | A of (int)

let stringToToken = new System.Collections.Generic.Dictionary<_,_>()
stringToToken.Add("A",15<token>)

let intToString = new System.Collections.Generic.Dictionary<_,_>()
intToString.Add(15,"A")
intToString.Add(0,"s")
intToString.Add(1,"k")

let private anyNonterm = -1<positionInGrammar>

let private terminalNums = new System.Collections.Generic.HashSet<_>()
terminalNums.Add(15<token>) |> ignore

let private stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<positionInGrammar>[]>()
stateAndTokenToNewState.Add(65536, [|14<positionInGrammar>; 12<positionInGrammar>|])
stateAndTokenToNewState.Add(196608, [|8<positionInGrammar>|])

let private outNonterms =
  [|[|1<positionInGrammar>,3<positionInGrammar>|];
    [|0<positionInGrammar>,13<positionInGrammar>|];
    [||];
    [|1<positionInGrammar>,4<positionInGrammar>|];
    [|1<positionInGrammar>,5<positionInGrammar>|];
    [|1<positionInGrammar>,6<positionInGrammar>|];
    [|1<positionInGrammar>,7<positionInGrammar>|];
    [|1<positionInGrammar>,2<positionInGrammar>|];
    [|1<positionInGrammar>,9<positionInGrammar>|];
    [|1<positionInGrammar>,10<positionInGrammar>|];
    [|1<positionInGrammar>,11<positionInGrammar>|];
    [|1<positionInGrammar>,2<positionInGrammar>|];
    [||];
    [|1<positionInGrammar>,12<positionInGrammar>|];
    [|1<positionInGrammar>,12<positionInGrammar>|] |]

let private startState = 0<positionInGrammar>

let private finalStates =
  new System.Collections.Generic.HashSet<int<positionInGrammar>>(
     [|2<positionInGrammar>;
       12<positionInGrammar>|])

let private nontermCount = 2

let private multipleInEdges = 
    [|false; false; true; false; false; false; false; false; false; false; false; false; true; false; false|]

let parserSource = new ParserSourceGLL (outNonterms, startState, finalStates, nontermCount, terminalNums, intToString, anyNonterm, stateAndTokenToNewState,stringToToken, multipleInEdges)


