module Yard.Generators.Common.FSA.Common


open System.Collections.Generic

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

open Microsoft.FSharp.Collections

[<Measure>] type state

let epsilon = "Epsilon"

[<StructuralEquality;StructuralComparison>]
type EdgeSymbol =
    | Term of string
    | Nonterm of int<state>
    | Epsilon of unit

type InternalFSA = {
    States             : (EdgeSymbol * int<state>) [] []
    Alphabet           : Set<EdgeSymbol>
    StateToNontermName : Dictionary<int<state>,string>
    StartState         : int<state>
    StartStates        : HashSet<int<state>> []
    //FirstStates        : int<state> list list
    LastStates         : HashSet<int<state>>
    FinalStates        : HashSet<int<state>>
    //Dictionary<int<state>, int>
}

let stateToString (nontermStringDict : Dictionary<int<state>, string>) state =
        let opt, value = nontermStringDict.TryGetValue state
        if opt then value else ""

let symbolToString nontermStringDict s = 
        match s with
        | Term term -> term
        | Nonterm nonterm -> stateToString nontermStringDict nonterm
        | Epsilon() -> "Epsilon"

let convertRulesToFSA (ruleList : Rule.t<Source.t,Source.t> list) =
    let states = new ResizeArray<(EdgeSymbol * int<state>) list>()
    let alphabet = new HashSet<EdgeSymbol>()
    let nonterms = new Dictionary<string, int<state>>()
    let stateToNontermName = new Dictionary<int<state>, string>()
    let dummyState = -1<state>
    let startComponent = ref -1

    let newEpsilonEdge (firstState : int<state>) (finalState : int<state>) = 
        () |> Epsilon |> alphabet.Add |> ignore    
        states.Item (int firstState) <- states.Item (int firstState) @ [Epsilon(), finalState]
    
    let newState() : int<state> = 
        states.Add []
        (states.Count-1) * 1<state>

    let sourse_tToSymbol isTerm (token : Source.t) =
        if isTerm
        then
            Term token.text |> alphabet.Add |> ignore
            Term token.text
        else
            let cond, value = nonterms.TryGetValue token.text
            if cond
            then
                Nonterm (value)
            else
                let state = newState()
                state |> Nonterm |> alphabet.Add |> ignore
                nonterms.Add (token.text,state)
                stateToNontermName.Add (state,token.text)
                Nonterm state

    let newEdge isTerm firstState (finalState : int<state> option) (s : Source.t option) =
        let symbol =
            if s.IsNone
            then
                Epsilon()
            else
                sourse_tToSymbol isTerm s.Value
        let final = ref dummyState
        if finalState.IsNone then
            final := newState()
            states.[int firstState] <- states.[int firstState] @ [symbol, !final]
        else
            final := finalState.Value
            states.[int firstState] <- states.[int firstState] @ [symbol, !final]
        !final

    let rec productionToStates (firstState : int<state>) (finalState : int<state> option) prod : int<state> =
        let nNewEdges firstState finalState prod = function 
            | 0  -> newEpsilonEdge firstState finalState
            | 1  -> productionToStates firstState (Some finalState) prod |> ignore
            | n  -> 
                let state = ref <| firstState
                for _ in 2..n do
                    let nextState = newState()
                    productionToStates !state (Some nextState) prod |> ignore
                    state := nextState
                productionToStates !state (Some finalState) prod |> ignore
            
        let rec proccessRepet expr (lower : _ option) (upper : _ option) =
            if lower.IsNone
            then
                proccessRepet expr (Some 0) upper
            elif upper.IsNone
            then
                let state = newState()
                nNewEdges firstState state expr lower.Value 
                if finalState.IsSome then
                    newEpsilonEdge state finalState.Value
                    productionToStates state (Some state) expr |> ignore
                    finalState.Value
                else
                    productionToStates state (Some state) expr
            elif (lower.Value > upper.Value) || (lower.Value < 0)
            then
                failwith "Incorrect parameters of range for Repeat!"
            elif lower.Value = upper.Value
            then
                if finalState.IsSome then
                    nNewEdges firstState finalState.Value expr lower.Value
                    finalState.Value
                else
                    if lower.Value = 0 then firstState else
                    let final = newState()
                    nNewEdges firstState final expr lower.Value
                    final
            else
                let lastState = ref <| firstState
                let final = if finalState.IsNone then newState() else finalState.Value
                if lower.Value <> 0 then 
                    lastState := newState()
                    nNewEdges firstState !lastState expr lower.Value

                for _ in lower.Value..upper.Value-2 do
                    let newLastState = newState()
                    productionToStates !lastState (Some newLastState) expr |> ignore
                    newEpsilonEdge !lastState final
                    lastState := newLastState
                    
                newEpsilonEdge !lastState final
                productionToStates !lastState (Some final) expr

        match prod with
        |PAlt (left, right) ->
            let finalState =
                if finalState.IsNone
                then
                    newState() |> Some
                else
                    finalState

            productionToStates firstState finalState left |> ignore
            productionToStates firstState finalState right
        | PSeq (s, _, _) ->
            let rec seqToStates first final = function
                | [hd] -> productionToStates first final hd.rule
                | hd :: tl ->
                    let newstate = productionToStates first None hd.rule
                    seqToStates newstate final tl
                | [] -> newEdge false first final None
            seqToStates firstState finalState s
        | PToken s | PLiteral s -> newEdge true firstState finalState (Some s)
        | PRef (rule,_) -> newEdge false firstState finalState (Some rule)
        | PMetaRef _ -> failwith "Metaref found!!!"
        | PMany expr -> proccessRepet expr (Some 0) None
        | PRepet (expr, lower, upper) -> proccessRepet expr lower upper
        | PSome expr -> proccessRepet expr (Some 1) None
        | POpt expr -> proccessRepet expr (Some 0) (Some 1)
        | x -> failwithf "Unexpected construction %A in grammar" x       
        
    ruleList
    |> List.iter (fun rule -> ignore <| sourse_tToSymbol false rule.name)

    let startStates, lastStates = 
        ruleList
        |> List.mapi (fun i rule ->
            match sourse_tToSymbol false rule.name with
            | Nonterm x ->
                let fnlState = productionToStates x (Some(newState())) rule.body
                if rule.isStart
                then 
                    startComponent := i
                [x], [fnlState]
            | _ -> failwith "???")
        |> List.mapi (fun i (x,y) ->
            new HashSet<_>(x), new HashSet<_>(y))
        |> Array.ofList
        |> Array.unzip
    
    let lastStates = 
        lastStates
        |> Array.fold (fun (x : HashSet<int<state>>) set ->
            x.UnionWith set
            x) (new HashSet<int<state>>())

    {   States = 
            states
            |> Seq.map Array.ofList
            |> Array.ofSeq;
        Alphabet = set alphabet;
        StateToNontermName = stateToNontermName;
        StartState = startComponent.Value * 1<state>;
        StartStates    = startStates;
        //FirstStates    = firstStates;
        FinalStates    = lastStates;
        LastStates     = new HashSet<_>(lastStates)}

/// Removes epsilon edges from FA.
let removeEpsilonEdges (fsa : InternalFSA) = 
    let epsilonClosure = Array.init (fsa.States.Length) (fun i -> new HashSet<_>())

    let rec getEpsilonClosure state =
        let currentSet = epsilonClosure.[int state]
        if currentSet.Count = 0
        then
            currentSet.Add(state) |> ignore
            fsa.States.[int state]
            |> Array.iter (fun (symbol, nextState) ->
                match symbol with
                |Epsilon() -> 
                    nextState
                    |> getEpsilonClosure
                    |> currentSet.UnionWith
                | _ -> ())
            currentSet

        else
            currentSet

    let startStates = new Dictionary<_,_>()

    fsa.StartStates
    |> Array.iteri (fun i states ->
        for st in states do
            startStates.Add(st, i))
    
    let finalStates = fsa.FinalStates
    let lastStates  = fsa.LastStates

    let newStates = 
        fsa.States
        |> Array.mapi (fun i edges ->
            edges
            |> Array.collect (fun (symbol, state) ->
                match symbol with
                | Epsilon() -> 
                    let cond, value = startStates.TryGetValue (i*1<state>)
                    if cond then
                        startStates.Add(state, value)
                    if finalStates.Contains state
                    then 
                        i*1<state> |> finalStates.Add |> ignore
                    [||]
                | _ ->
                    state
                    |> getEpsilonClosure
                    |> Seq.map (fun stateToAdd ->
                        symbol, stateToAdd)
                    |> Array.ofSeq)
            |> (fun x ->
                if Array.isEmpty x
                then (i*1<state>) |> lastStates.Add |> ignore
                x)
                )
    
    let startStates =
        startStates
        |> Seq.groupBy (fun x -> x.Value)
        |> Seq.sortBy (fun (groupN,_) -> groupN)
        |> Seq.map (fun (_,states) ->
            states
            |> Seq.map (fun x -> x.Key)
            |> (fun x -> new HashSet<_>(x)))
        |> Array.ofSeq

    {fsa with
        States = newStates;
        Alphabet = Epsilon() |> fsa.Alphabet.Remove;
        StartStates    = startStates;
        //FirstStates    = firstStates;
        FinalStates    = finalStates;
        LastStates     = lastStates}

/// Converts NFA without epsilon edges to DFA
let toDFA fsa = 
    let getOutSets (set: HashSet<int<state>>) =
        let newSets = new Dictionary<_,HashSet<_>>()
        for state in set do
            for symbol,nextState in fsa.States.[int state] do
                let cond, value = newSets.TryGetValue symbol
                if cond
                then
                    value.Add nextState |> ignore
                else
                    newSets.Add(symbol, new HashSet<_>([nextState]))
        newSets 
    
    let newStates = new ResizeArray<_>()
    let newStartStates = new ResizeArray<HashSet<_>>()
    let newStartState = ref -1<state>
    let startStates = fsa.StartStates
    let queue = new Queue<_>()
    let statesEliminationSet = new ResizeArray<HashSet<int<state>>>()

    startStates
    |> Array.iteri (fun i s ->
        queue.Enqueue s
        new HashSet<_>([statesEliminationSet.Count * 1<state>]) |> newStartStates.Add |> ignore
        if i = int fsa.StartState
        then
            newStartState := statesEliminationSet.Count * 1<state>
        statesEliminationSet.Add s
        )

    while queue.Count <> 0 do
        let setToProcess = queue.Dequeue()
        let newSets = getOutSets setToProcess
        let newEdges = new ResizeArray<_>()
        newStates.Add newEdges

        for keyValue in newSets do
            let symbol = keyValue.Key
            let states = keyValue.Value
            let stateExists = ref false

            statesEliminationSet
            |> ResizeArray.iteri (fun stateNum set ->
                if states.SetEquals(set)
                then
                    stateExists := true
                    newEdges.Add(symbol, stateNum))

            if !stateExists |> not
            then
                statesEliminationSet.Add(states)
                newEdges.Add(symbol, statesEliminationSet.Count - 1)
                queue.Enqueue(states)
        
    let newFinalStates = new HashSet<_>()
    let newLastStates = new HashSet<_>()
    let finalStates = fsa.FinalStates
    let lastStates = fsa.LastStates
    let stateToNewState = Array.create (fsa.States.Length) 0<state>

    statesEliminationSet
    |> ResizeArray.iteri (fun stateNum set ->
        for st in set do
            if finalStates.Contains st
            then 
                stateNum*1<state> |> newFinalStates.Add |> ignore

            if lastStates.Contains st
            then 
                stateNum*1<state> |> newLastStates.Add |> ignore
            stateToNewState.[int st] <- stateNum*1<state>)
            
    let newStates = 
        newStates
        |> ResizeArray.map(fun edges ->
            edges
            |> ResizeArray.map(fun (symbol, state) ->
                (match symbol with
                | Nonterm i -> stateToNewState.[int i] |> Nonterm
                | _ -> symbol), state*1<state>)
            |> ResizeArray.toArray)
        |> ResizeArray.toArray
    
    let stateToNontermName = new Dictionary<int<state>,string>()

    let newAlphabet = 
        fsa.Alphabet
        |> Seq.map (fun symbol ->
            match symbol with
            | Nonterm i -> 
                let cond,value = stateToNontermName.TryGetValue stateToNewState.[int i]
                if cond
                then
                    stateToNewState.[int i] |> stateToNontermName.Remove |> ignore
                    stateToNontermName.Add(stateToNewState.[int i], (value + " / " + fsa.StateToNontermName.[i]))
                else
                    stateToNontermName.Add(stateToNewState.[int i], fsa.StateToNontermName.[i])
                stateToNewState.[int i] |> Nonterm
            | _ -> symbol)
        |> set

    {fsa with 
        States = newStates;
        Alphabet = newAlphabet;
        StateToNontermName = stateToNontermName;
        StartState = !newStartState;
        StartStates = Array.ofSeq newStartStates;
        LastStates = newLastStates;
        FinalStates = newFinalStates}

/// Returns sets of equivalent states
/// http://goo.gl/z9uJP0
let findEquivalenceClasses fsa =  
    let inv = Array.init (fsa.States.Length) (fun i -> new Dictionary<_,ResizeArray<_>>())
    let buildInv() = 
        for i in 0..fsa.States.Length-1 do
            fsa.States.[i]
            |> Array.iter (fun (symbol, state) ->
                //let literal = symbolToString nontermStringDict symbol

                let cond, value = inv.[int state].TryGetValue symbol
                if cond then
                    value.Add i
                else
                    inv.[int state].Add(symbol, new ResizeArray<_>([i]))
                )
    buildInv()
    let Class = Array.create (fsa.States.Length) 1
    let classesP = new ResizeArray<_>()
    let finalSet = new ResizeArray<_>(fsa.LastStates)
    let others = new ResizeArray<_>()
    for state in 0..fsa.States.Length-1 do
        if finalSet.Contains (state*1<state>)
        then Class.[state] <- 0
        else others.Add (state*1<state>)
    classesP.Add(finalSet)
    classesP.Add(others)

    let queue = new Queue<_>()

    for c in fsa.Alphabet do
        queue.Enqueue(0,c)
        queue.Enqueue(1,c)

    while queue.Count <> 0 do
        let classNumber, symbol = queue.Dequeue()
        let statesOfCurrentClass = classesP.[classNumber]
        ///class -> prevStatesForCurrentClass
        let involved = new Dictionary<int,ResizeArray<int<state>>>()

        for curState in statesOfCurrentClass do
            if inv.[int curState].ContainsKey symbol then
                for prevState in inv.[int curState].[symbol] do
                    let classOfPrevState = Class.[prevState]
                    let cond, value = involved.TryGetValue classOfPrevState
                    if cond
                    then
                        value.Add (prevState*1<state>)
                    else involved.Add(classOfPrevState, new ResizeArray<_>([(prevState*1<state>)]))
            
        for keyValue in involved do
            let classOfPrevState, states = keyValue.Key, keyValue.Value
                
            if states.Count < classesP.[classOfPrevState].Count then
                let j = classesP.Count
                classesP.Add(new ResizeArray<_>())

                for r in states do
                    classesP.[classOfPrevState].Remove r |> ignore
                    classesP.[j].Add r

                if classesP.[j].Count > classesP.[classOfPrevState].Count
                    then 
                        let buf = classesP.[classOfPrevState]
                        classesP.[classOfPrevState] <- classesP.[j]
                        classesP.[j] <- buf

                for r in classesP.[j] do
                    Class.[int r] <- j
                for c in fsa.Alphabet do
                    queue.Enqueue(j,c)

    classesP

let minimizeFSA fsa = //(states : (EdgeSymbol * int<state>) [][]) (classes : ResizeArray<ResizeArray<int<state>>>) (nonterms : Set<_>) (stateStringDict : Dictionary<int<state>, string>) (startState:int<state>) (finalState:int<state>) = 
    let classes = findEquivalenceClasses fsa

    let sortClasses() =
        let newClasses = new ResizeArray<_>()
        let otherStatesClasses = new ResizeArray<_>()

        let nonterms =
            fsa.StartStates
            |> Array.fold (fun (x : HashSet<int<state>>) set ->
                x.UnionWith set
                x) (new HashSet<int<state>>())

        for classNumber in 0..classes.Count-1 do
            if classes.[classNumber].Count <> 0 then
                if classes.[classNumber].ToArray() |> Array.exists (fun x -> nonterms.Contains (x))
                then
                    newClasses.Add (classes.[classNumber])
                else
                    otherStatesClasses.Add (classes.[classNumber])
        
        newClasses.AddRange otherStatesClasses
        newClasses
        
    let classes = sortClasses()

    let stateToNewState = Array.create fsa.States.Length 0
    for classNumber in 0..classes.Count-1 do
        for stateNumber in classes.[classNumber] do
            stateToNewState.[int stateNumber] <- classNumber

    let newStatesSets = Array.init (classes.Count) (fun x -> new HashSet<_>())

    for classNumber in 0..classes.Count-1 do
        for stateNumber in classes.[classNumber] do
            for symbol,nextState in fsa.States.[int stateNumber] do
                let newNextState = stateToNewState.[int nextState]
                let newSymbol = 
                    match symbol with
                    | Nonterm nonterm -> stateToNewState.[int nonterm]*1<state> |> Nonterm
                    | _ -> symbol
                newStatesSets.[classNumber].Add (newSymbol,newNextState) |> ignore
    
    let statesToReturn = 
        newStatesSets
        |> Array.map (fun set ->
            [|for symbol,i in set -> symbol,i*1<state>|])
    
    let stateToNontermName = new Dictionary<_,_>()

    for keyValue in fsa.StateToNontermName do
        let newState = stateToNewState.[int keyValue.Key]
        let string = keyValue.Value

        let cond, value = stateToNontermName.TryGetValue (newState*1<state>)
        if cond
        then
            stateToNontermName.Remove(newState*1<state>) |> ignore
            stateToNontermName.Add(newState*1<state>, value + string)
        else
            stateToNontermName.Add(newState*1<state>, string)
    
    let newStartStates = 
        fsa.StartStates
        |> Array.map (fun states ->
            let newStates = new HashSet<_>()

            for st in states do
                stateToNewState.[int st]*1<state> |> newStates.Add |> ignore
            newStates)

    let newFinalStates = new HashSet<_>()
        
    for state in fsa.FinalStates do
        stateToNewState.[int state]*1<state> |> newFinalStates.Add |> ignore

    {fsa with
        States = statesToReturn;
        StateToNontermName = stateToNontermName;
        StartState = stateToNewState.[int fsa.StartState]*1<state>;
        StartStates    = newStartStates;
        FinalStates    = newFinalStates}
    //statesToReturn, nonterms.Count, newStateStringDict, (stateToNewState.[int startState]*1<state>), (stateToNewState.[int finalState]*1<state>)

let genFirstSet fsa =
    let nontermsCount = fsa.StartStates.Length
    let firstSet = new Dictionary<int<state>,HashSet<string>>()

    let addToResult state value = 
        let cond,set = firstSet.TryGetValue state
        if cond
        then set.UnionWith value
        else firstSet.Add(state, value) |> ignore
    (*
    let rec dfs (state: int<state>) (callStack : Set<_>) : int<state> list * HashSet<string> = 
        if firstSet.ContainsKey state then
            firstSet.[state]
        else
            let result = new HashSet<string>()

            for symbol,_ in fsa.States.[int state] do
                match symbol with
                | Nonterm nextNonterm -> 
                    if callStack.Contains nextNonterm
                    then result.Add 
                    else 
                        let res = 
                            let cond,value = firstSet.TryGetValue nextNonterm
                            if cond then value else
                            dfs nextNonterm nextNonterm (callStack.Add nonterm)
                        
                        addToResult nonterm res
                | Term term -> addToResult nonterm (new HashSet<_>([term]))
                | _ -> failwith "Epsilon edge found while build first set"
                //| Epsilon() -> dfs nonterm nextState callStack |> ignore
            
                //failwith "first set wasnt counted"
    
    for states in fsa.StartStates do
        for state in states do
            dfs state (Set.empty) |> ignore
    *)
    firstSet


let printDot filePrintPath fsa = 
    let strs = new ResizeArray<_>(["digraph G {\nnode [shape = circle]"])
    let finalStates = fsa.FinalStates
    let startStates = 
        fsa.StartStates
        |> Array.fold (fun (x : HashSet<int<state>>) set ->
            x.UnionWith set
            x) (new HashSet<int<state>>())
    fsa.States
    |> Array.iteri (fun i state ->
        let currState = i*1<state>
        let hd =
            let label = stateToString fsa.StateToNontermName currState
            
            if finalStates.Contains currState && fsa.StartState = currState
            then sprintf "%i[label=\"%s\", style=filled, fillcolor=brown]" currState label     
            elif fsa.StartState = currState
            then sprintf "%i[label=\"%s\", style=filled, fillcolor=green]" currState label
            elif finalStates.Contains currState
            then sprintf "%i[label=\"%s\", shape = doublecircle, style=filled, fillcolor=red]" currState label
            elif startStates.Contains currState
            then sprintf "%i[label=\"%s\", style=filled, fillcolor=yellow]" currState label
            elif label = ""
            then 
                //sprintf "%i[label=\"%i\", style=filled, fillcolor=brown]" currState currState
                sprintf "%i[label=\"\"]" currState 
            else sprintf "%i[label=\"%s\"]" currState label

        let tl = 
            state
            |> Array.map (fun (symbol,nextState) ->
                match symbol with
                | Epsilon() -> sprintf "%i -> %i [label=\"\",color=blue]; \n" i nextState
                | Term t -> sprintf "%i -> %i [label=\"%s\"]; \n" i nextState t
                | Nonterm n -> sprintf "%i -> %i [label=\"%s\",color=green]; \n" i nextState (symbolToString fsa.StateToNontermName symbol))
        
        strs.Add hd
        strs.AddRange tl
        )

    strs.Add "}"
    System.IO.File.WriteAllLines(filePrintPath, strs)
    fsa