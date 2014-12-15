namespace Yard.EBNF.NumberedRules

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.EBNF.GrammarWithNFARightSide
open Yard.EBNF.Indexator
open Yard.Generators.Common

type NumberedRulesEBNF (ruleList : Rule.t<Source.t,Source.t> list, indexator : IndexatorEBNF, caseSensitive) =
    let transformLiteral = IndexatorEBNF.transformLiteral caseSensitive
    let rules = ruleList |> Array.ofList
    let start =
        rules
        |> Array.findIndex (fun rule -> rule.isStart)
    let left = rules |> Array.map (fun x -> x.name.text |> indexator.nonTermToIndex)
    let right =
        let bodyToDFA body = 
            let nextStateNumber, vertexCount =
                let number = ref -1
                (fun () -> incr number; !number),
                (fun () -> !number + 1)
            let nextStateVertex (stateToVertex : ResizeArray<_>) = 
                let nextVertex = new Vertex<_,_>(nextStateNumber())
                stateToVertex.Add(nextVertex)
                nextVertex

            let rec regExToDFA (firstState : Vertex<_,_>)  (stateToVertex : ResizeArray<Vertex<_,_>>)  = function
                |PAlt (x,y) ->
                    let firstStateX = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(firstStateX, indexator.epsilonIndex))
                    let lastStateX : Vertex<_,_> = regExToDFA firstStateX stateToVertex x 
                    let firstStateY = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(firstStateY, indexator.epsilonIndex))
                    let lastStateY = regExToDFA firstStateY stateToVertex y
                    let lastState = nextStateVertex stateToVertex
                    lastStateX.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastStateY.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastState
                |PSeq (s, _, _) ->
                    match s with
                    | [] ->
                        let lastState = nextStateVertex stateToVertex
                        firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                        lastState
                    | _ ->                    
                        let rec seqToDFA fstState = function
                        | [] -> fstState
                        | x :: [] -> regExToDFA fstState stateToVertex x
                        | x :: xs -> 
                            let lastState = regExToDFA fstState stateToVertex x
                            let lstState = nextStateVertex stateToVertex
                            lastState.addEdge(new Edge<_,_>(lstState, indexator.epsilonIndex))
                            seqToDFA lstState xs
                         
                        s |> List.map (fun (x : elem<_,_>) -> x.rule) |> seqToDFA firstState
                |PToken _ | PLiteral _ | PRef _ as x -> 
                    let index =
                        match x with
                        | PToken token -> indexator.termToIndex token.text
                        | PLiteral lit -> indexator.literalToIndex <| transformLiteral lit.text
                        | PRef (nTerm, _) -> indexator.nonTermToIndex nTerm.text
                        | _ -> failwithf "Unexpected construction"
                    let lastState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(lastState, index))
                    lastState
                |PMany x -> 
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lstState = regExToDFA fstState stateToVertex x
                    lstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastState
                |PSome x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lstState = regExToDFA fstState stateToVertex x
                    lstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastState
                |POpt x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lstState = regExToDFA fstState stateToVertex x
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastState
                //|PMetaRef
                //|PRepet
                //|PPerm
                | x -> failwithf "Unexpected construction %A" x
             
            let stateToVertex = new ResizeArray<Vertex<_,_>>()
            let firstState = nextStateVertex stateToVertex
            regExToDFA firstState stateToVertex body|> ignore
            let prod : NFAProduction.t = {numberOfStates = vertexCount(); startState = firstState; stateToVertex = stateToVertex |> Seq.toArray}
            prod

        ruleList
        |> List.map
            (fun rule->
                let dfaRule : NFARule.t<_,_> =
                    {
                        name = rule.name;
                        args = rule.args;
                        body = bodyToDFA rule.body;
                        isStart = rule.isStart;
                        isPublic = rule.isPublic;
                        metaArgs = rule.metaArgs
                    }
                dfaRule
            )
        |> Array.ofList

    let rulesWithLeft =
        let result : int list array = Array.create indexator.fullCount []
        for i in 0..rules.Length-1 do
            result.[left.[i]] <- i::result.[left.[i]]
        result
        |> Array.map (List.rev >> Array.ofList)

    (*let errRulesExists = 
        let errInd = indexator.errorIndex
        let res = ref false
        for i in 0..right.GetLength(0)-1 do
            if not !res && Array.exists((=) errInd) right.[i] then
                res := true
        !res*)

    let symbolAndNextPos =
        let result : (int * int) [][] = Array.zeroCreate rules.Length
        for i in 0..rules.Length-1 do
            let nfa = right.[i].body
            result.[i] <- Array.create nfa.numberOfStates (indexator.epsilonIndex, 0)
            for j in 0..nfa.numberOfStates-1 do
                let rec getSymbol = function
                |[] -> ()
                |(x : Edge<_,_>)::xs -> 
                    if x.label <> indexator.epsilonIndex then result.[i].[j] <- (x.label, x.dest.label) else getSymbol xs
                nfa.stateToVertex.[j].outEdges |> List.ofSeq |> getSymbol 
        result
    
    member this.rulesCount = rules.Length
    member this.startRule = start
    member this.startSymbol = left.[start]
    member this.leftSide num = left.[num]
    member this.leftSideArr = left
    member this.rightSide num = right.[num].body
    member this.numberOfStates num = right.[num].body.numberOfStates
    member this.state rule pos = right.[rule].body.stateToVertex.[pos]
    member this.symbol rule pos = 
        let (symbol, _) = symbolAndNextPos.[rule].[pos]
        symbol
    member this.nextPos rule pos = 
        let (_, pos) = symbolAndNextPos.[rule].[pos]
        pos
    member this.rulesWithLeftSide symbol = rulesWithLeft.[symbol]
    //member this.errorRulesExists = errRulesExists
