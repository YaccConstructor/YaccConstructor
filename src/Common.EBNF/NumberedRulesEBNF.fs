namespace Yard.Generators.Common.EBNF

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Generators.Common

type TranslateBuilder (startRule) =
    
    let identN = ref 0
    let ruleCount = ref 0
    
    let builder = new System.Text.StringBuilder()

    member this.Append line =
        let ident = 
            String.replicate (!identN * 4) " "
        Printf.kprintf (fun s -> builder.Append(ident).Append(s).Append("\n") |> ignore) line

    static member FunPrefix = "_rnglr_rb_"

    member this.Finish() =
        this.Untab()
        this.Append ""
        this.PrintCall startRule

    member this.PrintBinding = 
        this.Append "let %s =" << Source.toString 

    member this.PrintCall n =
        this.Append "%s%d tree" TranslateBuilder.FunPrefix n

    member this.PrintIfNext num =
        this.Append "if currentEdge.Dest.Label = %d then" num

    member this.PrintElseNext() =
        this.Append "else"

    member this.PrintWhileNext num =
        this.Append "while currentEdge.Dest.Label = %d do" num
    
    member this.InitPrint() =
        this.Append("let _rnglr_rb_translate (tree : Tree<Token>)")
        this.Tab()

    member this.StartNewRule() =
        let bindingStart =
            if !ruleCount = 0 then
                "let rec"
            else
                this.Untab()
                "\nand"
        this.Append "%s %s%d (tree : Tree<_>) =" bindingStart TranslateBuilder.FunPrefix !ruleCount
        this.Tab()
        incr ruleCount

    override this.ToString() = builder.ToString()

    member this.Tab() =
        incr identN

    member this.Untab() =
        decr identN


type NumberedRulesEBNF (ruleList : Rule.t<Source.t,Source.t> list, indexator : IndexatorEBNF, caseSensitive, needTranslate) =
    let transformLiteral = IndexatorEBNF.transformLiteral caseSensitive
    let rules = ruleList |> Array.ofList
    let start =
        rules
        |> Array.findIndex (fun rule -> rule.isStart)
    let left = rules |> Array.map (fun x -> x.name |> Source.toString |> indexator.nonTermToIndex)
    
    let translateBuilder = 
        if needTranslate then
            Some <| new TranslateBuilder(start)
        else
            None

    let rulesWithLeft =
        let result : int array = Array.create indexator.fullCount -1
        for i in 0..rules.Length-1 do
            //expected one rule for one nonterminal
            //TODO: exception and constraint/conversion
            result.[left.[i]] <- i
        result

    let right =
        let bodyToDFA (body : Production.t<Source.t, Source.t>) = 
            let nextStateNumber, vertexCount =
                let number = ref -1
                (fun () -> incr number; !number),
                (fun () -> !number + 1)
            let nextStateVertex (stateToVertex : ResizeArray<_>) = 
                let nextVertex = new Vertex<_,_>(nextStateNumber())
                stateToVertex.Add(nextVertex)
                nextVertex

            if needTranslate then
                translateBuilder.Value.StartNewRule()

            let rec regExToDFA (firstState : Vertex<_,_>)  (stateToVertex : ResizeArray<Vertex<_,_>>)  = function
                |PAlt (x,y) ->
                    
                    let firstStateX = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(firstStateX, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.PrintIfNext firstStateX.label
                        translateBuilder.Value.Tab()
                    let lastStateX : Vertex<_,_> = regExToDFA firstStateX stateToVertex x 
                    
                    let firstStateY = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(firstStateY, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.Untab()
                        translateBuilder.Value.PrintElseNext()
                        translateBuilder.Value.Tab()
                    let lastStateY = regExToDFA firstStateY stateToVertex y
                    let lastState = nextStateVertex stateToVertex
                    lastStateX.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastStateY.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))

                    if needTranslate then
                        translateBuilder.Value.Untab()

                    lastState
                |PSeq (s, meta : Source.t option, _) ->
                    let lastState =
                        match s with
                        | [] ->
                            let lastState = nextStateVertex stateToVertex
                            firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                            //TODO
                            lastState
                        | _ ->                    
                            let rec seqToDFA fstState (xs : elem<Source.t,Source.t> list) =
                                match xs with
                                | [] -> fstState
                                | x :: [] -> 
                                    if needTranslate && x.binding.IsSome then
                                        translateBuilder.Value.PrintBinding x.binding.Value
                                        translateBuilder.Value.Tab()
                                    let lastState = regExToDFA fstState stateToVertex x.rule
                                    if needTranslate && x.binding.IsSome then
                                        translateBuilder.Value.Untab()
                                    lastState
                                | x :: xs -> 
                                    if needTranslate && x.binding.IsSome then
                                        translateBuilder.Value.PrintBinding x.binding.Value
                                        translateBuilder.Value.Tab()
                                    let lastState = regExToDFA fstState stateToVertex x.rule
                                    if needTranslate && x.binding.IsSome then
                                        translateBuilder.Value.Untab()
                                    let lstState = nextStateVertex stateToVertex
                                    lastState.addEdge(new Edge<_,_>(lstState, indexator.epsilonIndex))
                                    //TODO
                                    seqToDFA lstState xs
                         
                            s 
                            |> seqToDFA firstState
                    if needTranslate && meta.IsSome then
                        translateBuilder.Value.Append "%s" <| Source.toString meta.Value
                    lastState
                |PToken _ | PLiteral _ | PRef _ as x -> 
                    let index =
                        match x with
                        | PToken token -> indexator.termToIndex <| Source.toString token
                        | PLiteral lit -> indexator.literalToIndex << transformLiteral <| Source.toString lit
                        | PRef (nTerm, meta) -> 
                            let index = indexator.nonTermToIndex <| Source.toString nTerm
                            let index = rulesWithLeft.[index]
                            if needTranslate then
                                translateBuilder.Value.PrintCall index
                                if meta.IsSome then
                                    translateBuilder.Value.Append "%s" <| Source.toString meta.Value
                            index
                        | _ -> failwithf "Unexpected construction"
                    let lastState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(lastState, index))
                    lastState
                |PMany x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.PrintWhileNext fstState.label
                        translateBuilder.Value.Tab()
                    let lstState = regExToDFA fstState stateToVertex x
                    lstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.Untab()
                    lastState
                |PSome x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.PrintWhileNext fstState.label
                        translateBuilder.Value.Tab()
                    let lstState = regExToDFA fstState stateToVertex x
                    lstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.Untab()
                    lastState
                |POpt x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.PrintIfNext fstState.label
                        translateBuilder.Value.Tab()
                    let lstState = regExToDFA fstState stateToVertex x
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.Untab()
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
        |> List.mapi
            (fun i rule->
                bodyToDFA rule.body
            )
        |> fun x ->
            if needTranslate then 
                translateBuilder.Value.Finish()
            Array.ofList x

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
            let nfa = right.[i]
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
    member this.rightSide num = right.[num]
    member this.rightSideArr = right
    member this.numberOfStates num = right.[num].numberOfStates
    member this.state rule pos = right.[rule].stateToVertex.[pos]
    member this.symbol rule pos = 
        let (symbol, _) = symbolAndNextPos.[rule].[pos]
        symbol
    member this.nextPos rule pos = 
        let (_, pos) = symbolAndNextPos.[rule].[pos]
        pos
    member this.ruleWithLeftSide symbol = rulesWithLeft.[symbol]
    member this.ruleWithLeftSideArr = rulesWithLeft
    member this.TranslateRules = translateBuilder.Value.ToString()
    //member this.errorRulesExists = errRulesExists
