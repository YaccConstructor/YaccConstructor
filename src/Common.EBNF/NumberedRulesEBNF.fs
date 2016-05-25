namespace Yard.Generators.Common.EBNF

open Yard.Core.IL
open Yard.Core.IL.Production
open System.Collections.Generic
open Yard.Generators.Common

//translateToAst - mapping number of rule to name of Ast type to wrap derivation into its parent
type TranslateBuilder (translateToAst : string [] option) =
    
    let identN = ref 0
    let ruleCount = ref 0
    
    let builder = new System.Text.StringBuilder()

    member this.Append line =
        let ident = 
            String.replicate (!identN * 4) " "
        Printf.kprintf (fun s -> builder.Append(ident).Append(s).Append("\n") |> ignore) line

    static member FunPrefix = "translate_"

    member this.Finish() =
        this.Untab()
        this.Append ""
        this.Append "match tree.Ast with"
        this.Append "| Ast.Node (rule, derivation) ->"
        this.Tab()
        this.Append "match rule with"
        for i in 0 .. !ruleCount - 1 do
            let str =
                if translateToAst.IsSome then
                    sprintf "| %d -> translate_%d derivation |> %s |> box" i i (translateToAst.Value.[i])
                else
                    sprintf "| %d -> translate_%d derivation |> box" i i
            this.Append "%s" str
        this.Append "| _ -> invalidArg \"rule\" \"Is out of count\""
        this.Untab()
        this.Append "| Ast.Leaf token -> box token"
        
    member this.InitPrint() =
        this.Append("let translateAst (tree : Tree<'Token>) =")
        this.Tab()
        //function
        this.Append "let getChildrenFromAstEdge (astEdge : AstEdge<'Token>) ="
        this.Tab()
        this.Append "match astEdge.SubTree with"
        this.Append "| Ast.Node (_, c) -> c"
        this.Append "| _ -> failwithf \"Tried to get children from Tree.Leaf\""
        this.Untab()

        this.Append "let getChildTokenFromAstEdge (astEdge : AstEdge<'Token>) ="
        this.Tab()
        this.Append "match astEdge.SubTree with"
        this.Append "| Ast.Leaf (Some t) -> t"
        this.Append "| _ -> failwithf \"Tried to get children from Tree.Leaf\""
        this.Untab()

        this.Append ""

    member this.PrintBinding = 
        this.Append "let %s =" << Source.toString 

    member this.PrintCall n =
        this.Append "let children = getChildrenFromAstEdge derivation.[!i]"
        this.Append "%s%d children" TranslateBuilder.FunPrefix n

    member this.PrintIfNext num =
        this.Append "if derivation.[!i].NfaDest = %d then" num

    member this.PrintIgnore() =
        this.Append "|> ignore"

    member this.PrintElse() =
        this.Append "else"

    member this.PrintExpression (sourceExpr : Source.t) =
        this.Append "%s" <| (Source.toString sourceExpr).Trim()
    
    member this.PrintLoopList guardEdgeDest inLoopCode =
        this.Append "seq {"
        this.Tab()
        this.Append "while derivation.[!i].NfaDest = %d do" guardEdgeDest
        this.Tab()
        this.Append "let x ="
        this.Tab()
        this.PrintSkipEdge()
        inLoopCode()
        this.Untab()
        this.PrintSkipEdge()
        this.Append "yield x"
        this.Untab()
        this.Untab()
        this.Append "}"
        this.Append "|> List.ofSeq"
    
    member this.PrintOpt guardEdgeDest optCode =
        this.PrintIfNext guardEdgeDest
        this.Tab()
        this.PrintSkipEdge()
        optCode()
        this.Append "|> Some"
        this.Untab()
        this.PrintElse()
        this.Tab()
        this.Append "None"
        this.Untab()

    member this.PrintToken() =
        this.Append "getChildTokenFromAstEdge derivation.[!i] |> tokenData"

    (*member this.PrintWhileNext num =
        this.Append "while derivation.[!i].NfaDest = %d do" num*)

    member this.PrintSkipEdge() =
        this.Append "incr i"

    member this.PrintSkipEdgeAndReturn() =
        this.Append "|> (fun x -> incr i; x)"

    member this.StartNewRule() =
        let bindingStart =
            if !ruleCount = 0 then
                "let rec"
            else
                this.Untab()
                this.Append ""
                "and"
        this.Append "%s %s%d (derivation : AstEdge<'Token>[]) =" bindingStart TranslateBuilder.FunPrefix !ruleCount
        this.Tab()
        this.Append "let i = ref 0"
        incr ruleCount

    override this.ToString() = builder.ToString()

    member this.Tab() =
        incr identN

    member this.Untab() =
        decr identN
        if !identN = 0 then
            printfn "h"


type NumberedRulesEBNF (ruleList : Rule.t<Source.t,Source.t> list, indexator : IndexatorEBNF, caseSensitive, needTranslate, translateToAst : (string -> string) option) =
    let transformLiteral = IndexatorEBNF.transformLiteral caseSensitive
    let rules = ruleList |> Array.ofList
    let start =
        rules
        |> Array.findIndex (fun rule -> rule.isStart)
    let left = rules |> Array.map (fun x -> x.name |> Source.toString |> indexator.nonTermToIndex)
    
    let translateBuilder = 
        if needTranslate then
            let translateToAst =
                if translateToAst.IsSome then
                    rules |> Array.map (fun x -> x.name |> Source.toString |> translateToAst.Value) |> Some
                else
                    None
            let x = new TranslateBuilder(translateToAst)
            x.InitPrint()
            Some x
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
                        translateBuilder.Value.PrintSkipEdge()
                    let lastStateX : Vertex<_,_> = regExToDFA firstStateX stateToVertex x 
                    
                    let firstStateY = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(firstStateY, indexator.epsilonIndex))
                    if needTranslate then
                        translateBuilder.Value.Untab()
                        translateBuilder.Value.PrintElse()
                        translateBuilder.Value.Tab()
                        translateBuilder.Value.PrintSkipEdge()
                    let lastStateY = regExToDFA firstStateY stateToVertex y
                    let lastState = nextStateVertex stateToVertex
                    lastStateX.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastStateY.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))

                    if needTranslate then
                        translateBuilder.Value.Untab()
                        translateBuilder.Value.PrintSkipEdgeAndReturn()

                    lastState
                |PSeq (s, meta : Source.t option, _) ->
                    let lastState =
                        match s with
                        | [] ->
                            let lastState = nextStateVertex stateToVertex
                            firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                            //TODO
                            if needTranslate then
                                translateBuilder.Value.PrintSkipEdge()
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
                                    if needTranslate then
                                        if x.binding.IsSome then
                                            translateBuilder.Value.Untab()
                                        else
                                            translateBuilder.Value.PrintIgnore()
                                    lastState
                                | x :: xs -> 
                                    if needTranslate && x.binding.IsSome then
                                        translateBuilder.Value.PrintBinding x.binding.Value
                                        translateBuilder.Value.Tab()
                                    let lastState = regExToDFA fstState stateToVertex x.rule
                                    let lstState = nextStateVertex stateToVertex
                                    lastState.addEdge(new Edge<_,_>(lstState, indexator.epsilonIndex))
                                    if needTranslate then
                                        if x.binding.IsSome then
                                            translateBuilder.Value.Untab()
                                        else
                                            translateBuilder.Value.PrintIgnore()
                                        translateBuilder.Value.PrintSkipEdge()
                                        translateBuilder.Value.PrintSkipEdge()
                                    seqToDFA lstState xs                         
                            s 
                            |> seqToDFA firstState
                    if needTranslate && meta.IsSome then
                        translateBuilder.Value.PrintExpression meta.Value
                    lastState
                |PToken _ | PLiteral _ | PRef _ as x -> 
                    let index =
                        match x with
                        | PToken token -> 
                            let index = indexator.termToIndex <| Source.toString token
                            if needTranslate then
                                translateBuilder.Value.PrintToken()
                            index
                        | PLiteral lit -> 
                            let index = indexator.literalToIndex << transformLiteral <| Source.toString lit
                            if needTranslate then
                                translateBuilder.Value.PrintToken()
                            index
                        | PRef (nTerm, meta) -> 
                            let index = indexator.nonTermToIndex <| Source.toString nTerm
                            if needTranslate then
                                let rule = rulesWithLeft.[index]
                                translateBuilder.Value.PrintCall rule
                                if meta.IsSome then
                                    translateBuilder.Value.PrintExpression meta.Value
                            index
                        | _ -> failwithf "Unexpected construction"
                    let lastState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(lastState, index))                     
                    lastState
                |PMany x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lstState = ref fstState
                    let lstStateEv() = 
                        lstState := regExToDFA fstState stateToVertex x                        
                    if needTranslate then
                        translateBuilder.Value.PrintLoopList fstState.label lstStateEv
                    else
                        lstStateEv()
                    let lstState = !lstState
                    lstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    firstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastState
                |PSome x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lstState = ref fstState
                    let lstStateEv() = 
                        lstState := regExToDFA fstState stateToVertex x                        
                    if needTranslate then
                        translateBuilder.Value.PrintLoopList fstState.label lstStateEv
                    else
                        lstStateEv()
                    let lstState = !lstState
                    lstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lastState = nextStateVertex stateToVertex
                    lstState.addEdge(new Edge<_,_>(lastState, indexator.epsilonIndex))
                    lastState
                |POpt x ->
                    let fstState = nextStateVertex stateToVertex
                    firstState.addEdge(new Edge<_,_>(fstState, indexator.epsilonIndex))
                    let lstState = ref fstState
                    let lstStateEv() = 
                        lstState := regExToDFA fstState stateToVertex x                        
                    if needTranslate then
                        translateBuilder.Value.PrintOpt fstState.label lstStateEv
                    else
                        lstStateEv()
                    let lstState = !lstState
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
