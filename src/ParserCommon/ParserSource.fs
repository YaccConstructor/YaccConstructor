namespace Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common

type ParserSourceGLL ( outNonterms        : (int<positionInGrammar> * int<positionInGrammar>) [] []
                     , startState         : int<positionInGrammar>
                     , finalStates        : HashSet<int<positionInGrammar>>
                     , nontermCount       : int
                     , terminalNums       : HashSet<int>
                     //, numIsEpsilon       : int -> bool
                     , stateToNontermName : Dictionary<int<positionInGrammar>,string>
                     //, firstSet           : HashSet<int>
                     , anyNonterm         : int<positionInGrammar>
                     , stateAndTokenToNewState : Dictionary<int, int<positionInGrammar>>
                     ) =

    let getTermsDictionaryKey (state: int<positionInGrammar>) token = 
        int( (int state <<< 16) ||| (token - outNonterms.Length) )

    //member this.FirstSet           = firstSet
    member this.OutNonterms             = outNonterms
    member this.FinalStates             = finalStates
    member this.StartState              = startState
    member this.NonTermCount            = nontermCount
    member this.TerminalNums            = terminalNums
    //member this.NumIsEpsilon       = numIsEpsilon
    member this.StateToNontermName      = stateToNontermName
    member this.GetTermsDictionaryKey   = getTermsDictionaryKey
    member this.AnyNonterm              = anyNonterm
    member this.StateAndTokenToNewState = stateAndTokenToNewState