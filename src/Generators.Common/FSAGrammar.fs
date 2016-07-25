namespace Yard.Generators.Common.FSAGrammar

open System.Collections.Generic

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

[<Measure>] type state

type EdgeSymbol = 
    | Term of string
    | Nonterm of int<state>
    | Epsilon of unit

type FSAGrammar (ruleList : Rule.t<Source.t,Source.t> list) =
    let _states = new List<(EdgeSymbol * int<state>) list>()
    let _nonTerms = new Dictionary<string, int<state>>()

    let _stateStringDict = new Dictionary<int<state>, string>()

    let dummyState = -1<state>

    let _startState = ref dummyState
    let _finalState = ref dummyState

    let newEpsilonEdge firstState finalState = 
            _states.[int firstState] <- _states.[int firstState] @ [Epsilon(), finalState]
    
    let newState () : int<state> = 
            _states.Add []
            LanguagePrimitives.Int32WithMeasure (_states.Count-1)

    let convertRulesToFSA () =
        let sourse_tToSymbol isTerm (token : Source.t) =
            if isTerm
            then
                Term token.text
            else
                let cond, value = _nonTerms.TryGetValue token.text
                if cond
                then Nonterm (value)
                else let state = newState()
                     _nonTerms.Add (token.text,state)
                     _stateStringDict.Add (state,token.text)
                     Nonterm state

        let newEdge isTerm firstState finalState s =
            _states.[int firstState] <- _states.[int firstState] @ [sourse_tToSymbol isTerm s, finalState]

        let rec productionToStates (firstState : int<state>) (finalState : int<state>) =
            let nNewEdges firstState finalState prod = function 
                | 0  -> newEpsilonEdge firstState finalState
                | 1  -> productionToStates firstState finalState prod
                | n  -> let state = ref <| firstState
                        for _ in 2..n do
                            let nextState = newState()
                            productionToStates !state nextState prod
                            state := nextState
                        productionToStates !state finalState prod
            
            let rec proccessRepet expr (lower : _ option) (upper : _ option) =
                if lower.IsNone
                then
                    proccessRepet expr (Some 0) upper
                elif upper.IsNone
                then
                    let state = newState()
                    nNewEdges firstState state expr lower.Value 
                    newEpsilonEdge state finalState
                    productionToStates state state expr
                elif (lower.Value > upper.Value) || (lower.Value < 0)
                then
                    failwith "Incorrect parameters of range for Repeat!"
                elif lower.Value = upper.Value
                then
                    nNewEdges firstState finalState expr lower.Value
                else
                    let lastState = ref <| firstState
                    if lower.Value <> 0 then 
                        lastState := newState()
                        nNewEdges firstState !lastState expr lower.Value

                    for _ in lower.Value..upper.Value-2 do
                        let newLastState = newState()
                        productionToStates !lastState newLastState expr
                        newEpsilonEdge !lastState finalState
                        lastState := newLastState
                    
                    productionToStates !lastState finalState expr
                    newEpsilonEdge !lastState finalState

            function
            // Alternative (e1 | e2) of (t<'patt,'expr>) * (t<'patt,'expr>)
            |PAlt (left, right) ->
                productionToStates firstState finalState left
                productionToStates firstState finalState right
            // Sequence * attribute. (Attribute is always applied to sequence) of (elem<'patt,'expr>) list * 'expr option * DLabel option
            | PSeq (s, _, _) ->
                let rec seqToStates first final = function
                    | hd :: [] -> productionToStates first final hd.rule
                    | hd :: tl -> let newstate = newState()
                                  productionToStates first newstate hd.rule
                                  seqToStates newstate final tl
                    | [] -> failwith "Seq is empty"
                seqToStates firstState finalState s
            // Token itself. Final element of parsing. of Source.t 
            // Literal. We can use constants ("if" and "then" in ' .."if" expr "then" expr...') of Source.t 
            | PToken s | PLiteral s -> newEdge true firstState finalState s
            // Reference to other rule inside production. With an optional args list.  of Source.t * 'expr option
            | PRef (rule,_) -> newEdge false firstState finalState rule 
            // Reference to metarule inside production (mr<<x>> in rule "a: mr<<x>> y z") of Source.t * 'expr option * t<'patt,'expr> list 
            | PMetaRef _ -> failwith "metaref found"
            // expr* of (t<'patt,'expr>)
            |PMany expr -> (*let state = newState()
                           productionToStates state state expr
                           newEpsilonEdge firstState state
                           newEpsilonEdge state finalState*)
                           proccessRepet expr (Some 0) None
            // Extended regexp repetition, "man egrep" for details of (t<'patt,'expr>) * int option * int option
            | PRepet (expr, lower, upper) -> proccessRepet expr lower upper
            // expr+  of (t<'patt,'expr>)
            | PSome expr -> proccessRepet expr (Some 1) None
            // expr? of (t<'patt,'expr>)
            | POpt expr -> proccessRepet expr (Some 0) (Some 1)
            | x -> failwithf "Unexpected construction %A in grammar" x       
             
        ruleList
        |> List.iter (fun rule -> match sourse_tToSymbol false rule.name with
                                  | Nonterm x -> let finalState = newState()
                                                 productionToStates x finalState rule.body
                                                 if rule.isStart
                                                 then 
                                                     _startState := x
                                                     _finalState := finalState
                                  | _ -> failwith "???")
    
    let inlineNonterms () =
        let processedStates = new List<int<state>>()
        let copiedComponents = new List<int<state>>()

        let getCopyOfComponent state = 
            let startState = ref dummyState
            let finalState = ref dummyState
            let visited = new List<_>()

            if not <| copiedComponents.Contains state
            then 
                copiedComponents.Add state
                let rec findFinal state = 
                    if visited.Contains state  then () else
                    visited.Add state
                    if _states.[int state].Length = 0
                    then
                        finalState := state
                    else 
                        _states.[int state]
                        |> List.iter (fun (_,nextState) ->
                            findFinal nextState)

                findFinal state
                startState := state
            else
                let newStates = new Dictionary<int<state>,int<state>>()

                let rec copyComponent initState =
                    if visited.Contains initState then () else
                    visited.Add initState
                    if _states.[int initState].Length = 0
                    then
                        finalState := newStates.[initState]
                    else
                        let newEdges = 
                            _states.[int initState]
                            |> List.map (fun (symbol,nextState) ->
                                if not <| newStates.ContainsKey (nextState)
                                then newStates.Add (nextState, newState())
                                     copyComponent nextState
                                (symbol, newStates.[nextState]))

                        _states.[int newStates.[initState]] <- newEdges
            
                newStates.Add (state, newState())
                copyComponent state

                startState := newStates.[state]

            !startState, !finalState

        let rec doInline initState (callStack : int<state> list) : unit =
            let visited = new List<_>()

            let rec processEdges initState =
                //if _states.[int initState].Length = 0
                //    then if nextState <> dummyState then newEpsilonEdge initState nextState
                //else
                if visited.Contains initState then () else
                visited.Add initState
                let newEdges = 
                    _states.[int initState]
                    |> List.map (fun (symbol,nextState) ->
                        match symbol with
                        | Nonterm state -> if List.contains state callStack
                                           then
                                               processEdges nextState
                                               (symbol,nextState)
                                           else

                                           if not <| processedStates.Contains state
                                           then
                                               doInline state (callStack@[state])

                                           let startOfCopy, endOfCopy = getCopyOfComponent state
                                           newEpsilonEdge state startOfCopy
                                           newEpsilonEdge endOfCopy nextState
                                           processEdges nextState
                                           (Epsilon(), startOfCopy)
                        | Term _ | Epsilon _ -> processEdges nextState
                                                (symbol,nextState))
                _states.[int initState] <- newEdges
            processEdges initState
            processedStates.Add initState

        doInline !_startState [!_startState]
    
    let stateToString state =
        let opt, value = _stateStringDict.TryGetValue state
        if opt then value else ""

    let symbolToString s = 
        match s with
        | Term term -> term
        | Nonterm nonterm -> stateToString nonterm
        | Epsilon() -> "Eps"

    let _printDot filePrintPath = 
        let strs = 
            _states
            |> Seq.mapi (fun i state ->
                            let currState = LanguagePrimitives.Int32WithMeasure i
                            let hd = let label = stateToString currState
                                                 //currState.ToString()
                                     
                                     if !_startState = currState
                                     then
                                         sprintf "%i[label=\"%s\", style=filled, fillcolor=green]" currState label
                                     elif !_finalState = currState
                                     then
                                         sprintf "%i[label=\"%s\", shape = doublecircle, style=filled, fillcolor=red]" currState label
                                     elif label = "" then 
                                         //sprintf "%i[label=\"%i\", style=filled, fillcolor=brown]" currState currState
                                         sprintf "%i[label=\"\"]" currState 
                                     else
                                         sprintf "%i[label=\"%s\"]" currState label
                            let tl = 
                                state
                                |> List.map (fun (symbol,nextState) ->
                                    match symbol with
                                    | Epsilon() -> sprintf "%i -> %i [label=\"\",color=blue]; \n" i nextState
                                    | Term t -> sprintf "%i -> %i [label=\"%s\"]; \n" i nextState t
                                    | Nonterm n -> sprintf "%i -> %i [label=\"%s\",color=green]; \n" i nextState (symbolToString symbol))
                            hd::tl)
            |> List.ofSeq
            |> List.collect (fun x -> x)
        System.IO.File.WriteAllLines(filePrintPath ,"digraph G {\nnode [shape = circle]\n"::strs@["}"])
                                       
            
    do
        convertRulesToFSA ()
        _printDot @"C:\zgrviewer-0.10.0\dot\bio_grammar_before_inline.dot"
        //inlineNonterms()
        //_printDot @"C:\zgrviewer-0.10.0\dot\bio_grammar.dot"




    member this.printDot = _printDot
    member this.states = _states
    member this.startState = _startState
    member this.finalState = _finalState
    //member this.nontermStates = _nontermStates