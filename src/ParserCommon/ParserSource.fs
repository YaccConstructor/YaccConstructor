﻿namespace Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common

type ParserSourceGLL ( outNonterms        : (int<positionInGrammar> * int<positionInGrammar>) [] []
                     , startState         : int<positionInGrammar>
                     , finalStates        : HashSet<int<positionInGrammar>>
                     , nontermCount       : int
                     , terminalNums       : HashSet<int<token>>
                     , intToString        : Dictionary<int,string>
                     , anyNonterm         : int<positionInGrammar>
                     , stateAndTokenToNewState : Dictionary<int, int<positionInGrammar>>
                     , stringToToken      : Dictionary<string,int<token>>
                     , multipleInEdges    : bool []
                     ) =

    let getTermsDictionaryKey (state: int<positionInGrammar>) token = 
        int( (int state <<< 16) ||| (token - outNonterms.Length) )
    
    let strToToken str = 
        let isExist, value = stringToToken.TryGetValue(str)
        if isExist
        then
            value
        else
            failwith "Such string is not in a grammar alphabet."

    member this.OutNonterms             = outNonterms
    member this.FinalStates             = finalStates
    member this.StartState              = startState
    member this.NonTermCount            = nontermCount
    member this.TerminalNums            = terminalNums
    member this.IntToString             = intToString
    member this.GetTermsDictionaryKey   = getTermsDictionaryKey
    member this.AnyNonterm              = anyNonterm
    member this.StateAndTokenToNewState = stateAndTokenToNewState
    member this.StringToToken           = strToToken
    member this.MultipleInEdges         = multipleInEdges