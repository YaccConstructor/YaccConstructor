namespace Yard.Generators.GLL.ParserCommon
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
                     , stateAndTokenToNewState : Dictionary<int64, int<positionInGrammar>>
                     , stringToToken      : Dictionary<string,int<token>>
                     , multipleInEdges    : bool []
                     , ?rightSideToRule   : string -> int
                     ) =

    //let getTermsDictionaryKey (state: int<positionInGrammar>) token = 
    //    int( (int state <<< 16) ||| (token - outNonterms.Length) )
    
    let getTermsDictionaryKey (state: int<positionInGrammar>) token : int64 =
        ((int64 state <<< 32) ||| (int64 token - int64 outNonterms.Length))
    
    let strToToken str = 
        let isExist, value = stringToToken.TryGetValue(str)
        if isExist
        then
            value
        else
            //failwith "Such string is not in a grammar alphabet."
            -2<token>

    let rev (map : Map<int, string>) = 
            Map.fold(fun (m : Map<string, int>) k v -> m.Add(v, k)) Map.empty map

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
    //member this.RightSideToRule         = rightSideToRule.Value

    member this.NameToId = 
        rev (this.IntToString |> Seq.map (|KeyValue|)|> Map.ofSeq)