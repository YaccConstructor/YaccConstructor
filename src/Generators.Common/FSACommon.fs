module Yard.Generators.Common.FSA.Common


open System.Collections.Generic

open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Generators.Common.Epsilon
open Yard.Generators.Common.SymbolSets
open Yard.Generators.Common

[<Measure>] type state

let epsilon = "Epsilon"

[<StructuralEquality;StructuralComparison>]
type EdgeSymbol =
    | Term of string
    | Nonterm of int<state>
    | Epsilon of unit

let stateToString (nontermStringDict : Dictionary<int<state>, string>) state =
        let opt, value = nontermStringDict.TryGetValue state
        if opt then value else ""

let symbolToString nontermStringDict s = 
        match s with
        | Term term -> term
        | Nonterm nonterm -> stateToString nontermStringDict nonterm
        | Epsilon() -> "Epsilon"

let convertRulesToFSA (ruleList : Rule.t<Source.t,Source.t> list) =
    let states = new List<(EdgeSymbol * int<state>) list>()
    let alphabet = new HashSet<_>()
    //let terms = new HashSet<string>()
    let nonterms = new Dictionary<string, int<state>>()
    let nontermStringDict = new Dictionary<int<state>, string>()
    let dummyState = -1<state>
    let startState = ref dummyState
    let finalState = ref dummyState

    let newEpsilonEdge (firstState : int<state>) (finalState : int<state>) = 
        alphabet.Add epsilon |> ignore    
        states.Item (int firstState) <- states.Item (int firstState) @ [Epsilon(), finalState]
    
    let newState() : int<state> = 
        states.Add []
        (states.Count-1) * 1<state>

    let sourse_tToSymbol isTerm (token : Source.t) =
        alphabet.Add token.text |> ignore
        if isTerm
        then
            //terms.Add token.text |> ignore
            Term token.text
        else
            let cond, value = nonterms.TryGetValue token.text
            if cond
            then
                Nonterm (value)
            else
                let state = newState()
                nonterms.Add (token.text,state)
                nontermStringDict.Add (state,token.text)
                Nonterm state

    let newEdge isTerm firstState (finalState : int<state> option) (s : Source.t option) =
        let symbol =
            if s.IsNone
            then
                Epsilon()
            else
                sourse_tToSymbol isTerm s.Value
        let final = ref dummyState
        let nextState = 
            states.[int firstState]
            |> List.tryFind (fun ((s:EdgeSymbol), next) -> s.Equals symbol)
        if finalState.IsNone then
            if nextState.IsNone then
                final := newState()
                states.[int firstState] <- states.[int firstState] @ [symbol, !final]
            else
                final := snd nextState.Value
        else
            if nextState.IsNone then
                final := finalState.Value
                states.[int firstState] <- states.[int firstState] @ [symbol, !final]
            else
                if finalState.Value <> snd nextState.Value then
                    newEpsilonEdge (snd nextState.Value) finalState.Value
                final := finalState.Value
            
        !final

    let rec productionToStates (firstState : int<state>) (finalState : int<state> option) prod : int<state> =
        let nNewEdges firstState finalState prod = function 
            | 0  -> newEpsilonEdge firstState finalState
            | 1  -> productionToStates firstState (Some finalState) prod |> ignore
            | n  -> let state = ref <| firstState
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
        // Alternative (e1 | e2) of (t<'patt,'expr>) * (t<'patt,'expr>)
        |PAlt (left, right) ->
            productionToStates firstState finalState left |> ignore
            productionToStates firstState finalState right
        // Sequence * attribute. (Attribute is always applied to sequence) of (elem<'patt,'expr>) list * 'expr option * DLabel option
        | PSeq (s, _, _) ->
            let rec seqToStates first final = function
                | hd :: [] -> productionToStates first final hd.rule
                | hd :: tl ->
                    let newstate = productionToStates first None hd.rule
                    seqToStates newstate final tl
                | [] -> newEdge false first final None
            seqToStates firstState finalState s
        // Token itself. Final element of parsing. of Source.t 
        // Literal. We can use constants ("if" and "then" in ' .."if" expr "then" expr...') of Source.t 
        | PToken s | PLiteral s -> newEdge true firstState finalState (Some s)
        // Reference to other rule inside production. With an optional args list.  of Source.t * 'expr option
        | PRef (rule,_) -> newEdge false firstState finalState (Some rule )
        // Reference to metarule inside production (mr<<x>> in rule "a: mr<<x>> y z") of Source.t * 'expr option * t<'patt,'expr> list 
        | PMetaRef _ -> failwith "Metaref found!!!"
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
    |> List.iter (fun rule -> ignore <| sourse_tToSymbol false rule.name)

    let finalStatesOfComponents = 
        ruleList
        |> List.map (fun rule ->
            match sourse_tToSymbol false rule.name with
            | Nonterm x ->
            let fnlState = productionToStates x (Some(newState())) rule.body
            if rule.isStart
            then 
                startState := x
                finalState := fnlState
            fnlState
            | _ -> failwith "???")

    states
    |> Seq.map(fun x -> Array.ofList x)
    |> Array.ofSeq
    , alphabet, new Set<_>(nonterms.Values), nontermStringDict, !startState, !finalState, finalStatesOfComponents

let findEquivalenceClasses (states : (EdgeSymbol * int<state>)[][]) (finalStatesOfComponents : int<state> list) alphabet nontermStringDict  = 
    let Inv = Array.init (states.Length) (fun i -> new Dictionary<string,List<_>>())//Array.init (alphabet.Count) (fun _ -> new List<_>()))
    let buildInv() = 
        for i in 0..states.Length-1 do
            states.[i]
            |> Array.iter (fun (symbol, state) ->
                let literal = symbolToString nontermStringDict symbol

                let cond, value = Inv.[int state].TryGetValue literal
                if cond then
                    value.Add i
                else
                    Inv.[int state].Add(literal, new List<_>([i]))
                )
    buildInv()
    let Class = Array.create (states.Length) 1
    let classesP = new List<_>()
    let F = new List<_>(finalStatesOfComponents)
    let others = new List<_>()
    for state in 0..states.Length-1 do
        if F.Contains (state*1<state>)
        then Class.[state] <- 0
        else others.Add (state*1<state>)
    classesP.Add(F)
    classesP.Add(others)

    let queue = new Queue<_>()

    for c in alphabet do
        queue.Enqueue(0,c)
        queue.Enqueue(1,c)

    while queue.Count <> 0 do
        let classNumber, symbol = queue.Dequeue()
        let statesOfCurrentClass = classesP.[classNumber]
        ///class -> prevStatesForCorrentClass
        let Involved = new Dictionary<int,List<int<state>>>()

        for curState in statesOfCurrentClass do
            if Inv.[int curState].ContainsKey symbol then
                for prevState in Inv.[int curState].[symbol] do
                    let classOfPrevState = Class.[prevState]
                    let cond, value = Involved.TryGetValue classOfPrevState
                    if cond
                    then
                        value.Add (prevState*1<state>)
                    else Involved.Add(classOfPrevState, new List<_>([(prevState*1<state>)]))
            
        for keyValue in Involved do
            let classOfPrevState, states = keyValue.Key, keyValue.Value
                
            if states.Count < classesP.[classOfPrevState].Count then
                let j = classesP.Count
                classesP.Add(new List<_>())

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
                for c in alphabet do
                    queue.Enqueue(j,c)

    classesP

let buildEquivalentFSA (states : (EdgeSymbol * int<state>) [][]) alphabet (classes : List<List<int<state>>>) (nonterms : Set<_>) (stateStringDict : Dictionary<int<state>, string>) (startState:int<state>) (finalState:int<state>) = 
    let sortClasses() =
        let newClasses = new List<_>()
        let otherStatesClasses = new List<_>()

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

    let stateToNewState = Array.create states.Length 0
    for classNumber in 0..classes.Count-1 do
        for stateNumber in classes.[classNumber] do
            stateToNewState.[int stateNumber] <- classNumber

    let newStatesSets = Array.init (classes.Count) (fun x -> new HashSet<_>())

    for classNumber in 0..classes.Count-1 do
        for stateNumber in classes.[classNumber] do
            let edges = states.[int stateNumber]
            for symbol,nextState in edges do
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
    
    let newStateStringDict = new Dictionary<_,_>()

    for keyValue in stateStringDict do
        let newState = stateToNewState.[int keyValue.Key]
        let string = keyValue.Value

        let cond, value = newStateStringDict.TryGetValue (newState*1<state>)
        if cond
        then
            newStateStringDict.Remove(newState*1<state>) |> ignore
            newStateStringDict.Add(newState*1<state>, value + string)
        else
            newStateStringDict.Add(newState*1<state>, string)

    statesToReturn, nonterms.Count, newStateStringDict, (stateToNewState.[int startState]*1<state>), (stateToNewState.[int finalState]*1<state>)

let genFirstSet (states: (EdgeSymbol*int<state>)[][]) nontermsCount =
    let firstSet = new Dictionary<int<state>,HashSet<string>>()

    let addToResult state value = 
        let cond,set = firstSet.TryGetValue state
        if cond
        then set.UnionWith value
        else firstSet.Add(state, value) |> ignore

    let rec dfs (nonterm: int<state>) (state: int<state>) (callStack : Set<_>) : HashSet<string>= 
        for symbol,nextState in states.[int state] do
            match symbol with
            | Nonterm nextNonterm -> 
                if callStack.Contains nextNonterm
                then ()
                else 
                    let res = 
                        let cond,value = firstSet.TryGetValue nextNonterm
                        if cond then value else
                        dfs nextNonterm nextNonterm (callStack.Add nonterm)
                        
                    addToResult nonterm res
            | Term term -> addToResult nonterm (new HashSet<_>([term]))
            | Epsilon() -> dfs nonterm nextState callStack |> ignore

        if firstSet.ContainsKey nonterm then
            firstSet.[nonterm]
        else
            failwith "sdfsf"

    for i in 0..nontermsCount - 1 do
        dfs (i * 1<state>) (i * 1<state>) (Set.empty) |> ignore

    firstSet


let printDot filePrintPath (states : (EdgeSymbol*int<state>)[][]) startState finalState stateStringDict = 
    let strs = new List<_>(["digraph G {\nnode [shape = circle]"])
    
    states
    |> Array.iteri (fun i state ->
        let currState = i*1<state>
        let hd =
            let label = stateToString stateStringDict currState
                        //currState.ToString()
                                     
            if startState = currState
            then
                sprintf "%i[label=\"%s\", style=filled, fillcolor=green]" currState label
            elif finalState = currState
            then
                sprintf "%i[label=\"%s\", shape = doublecircle, style=filled, fillcolor=red]" currState label
            elif label = "" then 
                //sprintf "%i[label=\"%i\", style=filled, fillcolor=brown]" currState currState
                sprintf "%i[label=\"\"]" currState 
            else
                sprintf "%i[label=\"%s\"]" currState label

        let tl = 
            state
            |> Array.map (fun (symbol,nextState) ->
                match symbol with
                | Epsilon() -> sprintf "%i -> %i [label=\"\",color=blue]; \n" i nextState
                | Term t -> sprintf "%i -> %i [label=\"%s\"]; \n" i nextState t
                | Nonterm n -> sprintf "%i -> %i [label=\"%s\",color=green]; \n" i nextState (symbolToString stateStringDict symbol))
        
        strs.Add hd
        strs.AddRange tl
        )

    strs.Add "}"
    System.IO.File.WriteAllLines(filePrintPath, strs)