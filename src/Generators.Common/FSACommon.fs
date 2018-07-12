module Yard.Generators.Common.FSA.Common

open System.Collections.Generic
open Microsoft.FSharp.Collections

open Yard.Core.IL
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common

[<StructuralEquality;StructuralComparison>]
type EdgeSymbol =
    | Term of string
    | Nonterm of int<positionInGrammar>
    | Epsilon of unit

type InternalFSA = {
    States             : (EdgeSymbol * int<positionInGrammar>) [] []
    Alphabet           : Set<EdgeSymbol>
    StateToNontermName : Dictionary<int<positionInGrammar>,string>
    StartComponentNumber         : int
    StartStates        : HashSet<int<positionInGrammar>> []
    //FirstStates        : int<positionInGrammar> list list
    //LastStates         : HashSet<int<positionInGrammar>>
    FinalStates        : HashSet<int<positionInGrammar>>
    //Dictionary<int<positionInGrammar>, int>
}

let stateToString (nontermStringDict : Dictionary<int<positionInGrammar>, string>) state =
        let opt, value = nontermStringDict.TryGetValue state
        if opt then value else ""

let symbolToString nontermStringDict s = 
        match s with
        | Term term -> term
        | Nonterm nonterm -> stateToString nontermStringDict nonterm
        | Epsilon() -> "Epsilon"

let convertRulesToFSA (ruleList : Rule<Source,Source> list) =
    let states = new ResizeArray<(EdgeSymbol * int<positionInGrammar>) list>()
    let alphabet = new HashSet<EdgeSymbol>()
    let nonterms = new Dictionary<string, int<positionInGrammar>>()
    let stateToNontermName = new Dictionary<int<positionInGrammar>, string>()
    let dummyState = -1<positionInGrammar>
    let startComponent = ref -1

    let newEpsilonEdge (firstState : int<positionInGrammar>) (finalState : int<positionInGrammar>) = 
        () |> Epsilon |> alphabet.Add |> ignore    
        states.Item (int firstState) <- states.Item (int firstState) @ [Epsilon(), finalState]
    
    let newState() : int<positionInGrammar> = 
        states.Add []
        (states.Count-1) * 1<positionInGrammar>

    let sourse_tToSymbol isTerm (token : Source) =
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

    let newEdge isTerm firstState (finalState : int<positionInGrammar> option) (s : Source option) =
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

    let rec productionToStates (firstState : int<positionInGrammar>) (finalState : int<positionInGrammar> option) prod : int<positionInGrammar> =
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
        |> Array.fold (fun (x : HashSet<int<positionInGrammar>>) set ->
            x.UnionWith set
            x) (new HashSet<int<positionInGrammar>>())

    {   States = 
            states
            |> Seq.map Array.ofList
            |> Array.ofSeq;
        Alphabet = set alphabet;
        StateToNontermName = stateToNontermName;
        StartComponentNumber = startComponent.Value;
        StartStates    = startStates;
        //FirstStates    = firstStates;
        FinalStates    = lastStates;
        //LastStates     = new HashSet<_>(lastStates)}
        }

/// Removes epsilon edges from FA using epsilon closure.
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

    let startStatesToComponentNum = new Dictionary<_,_>()

    fsa.StartStates
    |> Array.iteri (fun componentNum states ->
        for st in states do
            startStatesToComponentNum.Add(st, componentNum))
    
    let finalStates = fsa.FinalStates
    //let lastStates  = fsa.LastStates

    let newStates = 
        fsa.States
        |> Array.mapi (fun currentState edges ->
            edges
            |> Array.collect (fun (symbol, nextState) ->
                match symbol with
                | Epsilon() -> 
                    let isStartState, componentNum = startStatesToComponentNum.TryGetValue (currentState*1<positionInGrammar>)
                    if isStartState && startStatesToComponentNum.ContainsKey nextState |> not then
                        startStatesToComponentNum.Add(nextState, componentNum)
                    if finalStates.Contains nextState
                    then 
                        currentState*1<positionInGrammar> |> finalStates.Add |> ignore
                    [||]
                | _ ->
                    nextState
                    |> getEpsilonClosure
                    |> Seq.map (fun stateToAdd ->
                        symbol, stateToAdd)
                    |> Array.ofSeq)
            |> (fun x ->
                if Array.isEmpty x
                then (currentState*1<positionInGrammar>) |> ignore //lastStates.Add |> ignore
                x)
                )
    
    let startStates =
        startStatesToComponentNum
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
        //LastStates     = lastStates
        }

/// Converts NFA without epsilon edges to DFA
let toDFA fsa = 
    let getOutSets (set: HashSet<int<positionInGrammar>>) =
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
    let newStartComponentNumber = ref -1
    let startStates = fsa.StartStates
    let queue = new Queue<_>()
    let statesEliminationSet = new ResizeArray<HashSet<int<positionInGrammar>>>()

    startStates
    |> Array.iteri (fun i s ->
        queue.Enqueue s
        /// here it is. each component has only one startState
        new HashSet<_>([statesEliminationSet.Count * 1<positionInGrammar>]) |> newStartStates.Add |> ignore
        if i = int fsa.StartComponentNumber
        then
            newStartComponentNumber := statesEliminationSet.Count
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
    //let lastStates = fsa.LastStates
    let stateToNewState = Array.create (fsa.States.Length) 0<positionInGrammar>

    statesEliminationSet
    |> ResizeArray.iteri (fun stateNum set ->
        for st in set do
            if finalStates.Contains st
            then 
                stateNum*1<positionInGrammar> |> newFinalStates.Add |> ignore

            (*
            if lastStates.Contains st
            then 
                stateNum*1<positionInGrammar> |> newLastStates.Add |> ignore
            stateToNewState.[int st] <- stateNum*1<positionInGrammar>
            *)
            )
            
    let newStates = 
        newStates
        |> ResizeArray.map(fun edges ->
            edges
            |> ResizeArray.map(fun (symbol, state) ->
                (match symbol with
                | Nonterm i -> stateToNewState.[int i] |> Nonterm
                | _ -> symbol), state*1<positionInGrammar>)
            |> ResizeArray.toArray)
        |> ResizeArray.toArray
    
    let stateToNontermName = new Dictionary<int<positionInGrammar>,string>()

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
        StartComponentNumber = !newStartComponentNumber;
        StartStates = Array.ofSeq newStartStates;
        //LastStates = newLastStates;
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
    //let finalSet = new ResizeArray<_>(fsa.LastStates)
    let finalSet = new ResizeArray<_>(fsa.FinalStates)
    let others = new ResizeArray<_>()
    for state in 0..fsa.States.Length-1 do
        if finalSet.Contains (state*1<positionInGrammar>)
        then Class.[state] <- 0
        else others.Add (state*1<positionInGrammar>)
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
        let involved = new Dictionary<int,ResizeArray<int<positionInGrammar>>>()

        for curState in statesOfCurrentClass do
            if inv.[int curState].ContainsKey symbol then
                for prevState in inv.[int curState].[symbol] do
                    let classOfPrevState = Class.[prevState]
                    let cond, value = involved.TryGetValue classOfPrevState
                    if cond
                    then
                        value.Add (prevState*1<positionInGrammar>)
                    else involved.Add(classOfPrevState, new ResizeArray<_>([(prevState*1<positionInGrammar>)]))
            
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
    |> ResizeArray.map(fun x -> Array.ofSeq x)

let minimizeFSA fsa =
    let classes = findEquivalenceClasses fsa

    let nonterms =
            fsa.StartStates
            |> Array.fold (fun (x : HashSet<int<positionInGrammar>>) set ->
                x.UnionWith set
                x) (new HashSet<int<positionInGrammar>>())

    let divideClassesWithMultipleNonterminals () = 
        let newClasses = new ResizeArray<_>()
        for classNumber in 0..classes.Count-1 do
            if classes.[classNumber].Length <> 0 then
                let currClass = classes.[classNumber]
                let nontermsInCurrentClass = 
                    currClass
                    |> Array.filter(fun x -> nonterms.Contains (x))
                if nontermsInCurrentClass.Length > 1 then 
                    nontermsInCurrentClass.[1..]
                    |> Array.iter (fun x -> 
                        newClasses.Add([|x|]))
                    newClasses.Add(currClass
                                   |> Array.filter(fun x -> 
                                        not(x <> nontermsInCurrentClass.[0] && (nonterms.Contains(x)))))
                else
                    newClasses.Add(currClass)
        newClasses
    
    let classes = divideClassesWithMultipleNonterminals()
    

    // move classes that contain nonterminals to beginning
    let sortClasses() =
        let newClasses = new ResizeArray<_>()
        let otherStatesClasses = new ResizeArray<_>()

        for classNumber in 0..classes.Count-1 do
            if classes.[classNumber].Length <> 0 then
                if classes.[classNumber] |> Array.exists (fun x -> nonterms.Contains (x))
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
                    | Nonterm nonterm -> stateToNewState.[int nonterm]*1<positionInGrammar> |> Nonterm
                    | _ -> symbol
                newStatesSets.[classNumber].Add (newSymbol,newNextState) |> ignore
    
    let statesToReturn = 
        newStatesSets
        |> Array.map (fun set ->
            [|for symbol,i in set -> symbol,i*1<positionInGrammar>|])
    
    let stateToNontermName = new Dictionary<_,_>()

    for keyValue in fsa.StateToNontermName do
        let newState = stateToNewState.[int keyValue.Key]
        let string = keyValue.Value

        let cond, value = stateToNontermName.TryGetValue (newState*1<positionInGrammar>)
        if cond
        then
            stateToNontermName.Remove(newState*1<positionInGrammar>) |> ignore
            stateToNontermName.Add(newState*1<positionInGrammar>, value + "|||" + string)
        else
            stateToNontermName.Add(newState*1<positionInGrammar>, string)
    
    let newStartStates = 
        fsa.StartStates
        |> Array.map (fun states ->
            let newStates = new HashSet<_>()

            for st in states do
                stateToNewState.[int st]*1<positionInGrammar> |> newStates.Add |> ignore
            newStates)

    let newFinalStates = new HashSet<_>()
        
    for state in fsa.FinalStates do
        stateToNewState.[int state]*1<positionInGrammar> |> newFinalStates.Add |> ignore

    let newStartComponentNum = 
        let oldStateFromOldComponent = 
            fsa.StartStates.[fsa.StartComponentNumber]
            |> Seq.find(fun _ -> true)
        
        let newStateFromOldComponent = stateToNewState.[int oldStateFromOldComponent] * 1<positionInGrammar>

        newStartStates
        |> Array.findIndex(fun x -> x.Contains(newStateFromOldComponent))


    {fsa with
        States = statesToReturn;
        StateToNontermName = stateToNontermName;
        StartComponentNumber = newStartComponentNum;
        StartStates    = newStartStates;
        FinalStates    = newFinalStates}
    //statesToReturn, nonterms.Count, newStateStringDict, (stateToNewState.[int startState]*1<positionInGrammar>), (stateToNewState.[int finalState]*1<positionInGrammar>)

let printDot filePrintPath fsa = 
    let strs = new ResizeArray<_>(["digraph G {\nnode [shape = circle]"])
    let startStates = 
        fsa.StartStates
        |> Array.fold (fun (x : HashSet<int<positionInGrammar>>) set ->
            x.UnionWith set
            x) (new HashSet<int<positionInGrammar>>())
    startStates.ExceptWith(fsa.StartStates.[fsa.StartComponentNumber])
    let initialStates = 
        fsa.StartStates.[fsa.StartComponentNumber]
        
    fsa.States
    |> Array.iteri (fun i state ->
        let currState = i*1<positionInGrammar>
        let hd =
            let label = stateToString fsa.StateToNontermName currState
            
            if fsa.FinalStates.Contains currState && (startStates.Contains currState || initialStates.Contains currState )
            then sprintf "%i[label=\"%s\", style=filled, fillcolor=brown]" currState label     
            elif initialStates.Contains currState
            then sprintf "%i[label=\"%s\", style=filled, fillcolor=green]" currState label
            elif fsa.FinalStates.Contains currState
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