namespace Yard.Generators.Common.GrammarFlowGraph

open System.Collections.Generic

open Yard.Core.IL
open Yard.Core.IL.Production
open QuickGraph

type GFGEdge =
    | Call of int   // stores matching return node
    | Scan of string
    | Eps           // other edges (Entry, Return, etc.)
    override this.ToString() =
        match this with
        | Call n -> string n
        | Scan s -> s
        | Eps -> ""

type GrammarFlowGraph (ruleList : Rule.t<Source.t,Source.t> list) as this =
    inherit AdjacencyGraph<int, TaggedEdge<int, GFGEdge>>()

    let nonTermToStates = new Dictionary<string, int * int>()  // nonTerm -> start/end nodes
    let stateToString = new Dictionary<int, string>()          // for visualization (TODO)

    let dummyState = -1
    let initState = ref dummyState
    let finalState = ref dummyState

    let newState () =
        let count = this.VertexCount
        this.AddVertex count |> ignore
        count                   
    
    let addEdge source target tag = 
        this.AddEdge (new TaggedEdge<_, _>(source, target, tag)) |> ignore

    let newEdges isTerm source (target : int option) (s: Source.t) =
        let final = ref dummyState
        if target.IsNone then final := newState() else final := target.Value
        if isTerm
        then addEdge source !final (Scan s.text)
        else
            let startState, endState = nonTermToStates.[s.text]
            let returnState = ref dummyState
            if target.IsNone then returnState := newState() else returnState := target.Value
            addEdge source startState (Call !returnState)
            addEdge endState !returnState Eps
            final := !returnState
        !final

    let rec productionToStates startState (endState: int option) prod : int =
        match prod with
        | PAlt (left, right) -> 
            productionToStates startState endState left |> ignore
            productionToStates startState endState right
        | PSeq (seq, _ , _) ->
            let entryState, exitState = newState(), newState()
            addEdge startState entryState Eps
            addEdge exitState endState.Value Eps 
            let rec seqToStates source target = function
                | hd :: [] -> productionToStates source target hd.rule
                | hd :: tl -> let newstate = productionToStates source None hd.rule
                              seqToStates newstate target tl
                | [] -> failwith "PSeq is empty"
            seqToStates entryState (Some exitState) seq
        | PToken s | PLiteral s -> newEdges true startState endState s
        | PRef (rule, _) -> newEdges false startState endState rule  
        | _ -> failwith "Unexpected PType"

    let rulesToGFG () =
        ruleList
        |> List.iter ( fun r -> if not (nonTermToStates.ContainsKey (r.name.text)) 
                                then nonTermToStates.Add (r.name.text, (newState(), newState())) )
                               
        for rule in ruleList do
            let startState, endState = nonTermToStates.[rule.name.text]
            productionToStates startState (Some endState) rule.body |> ignore
            if rule.isStart 
            then 
                initState := startState
                finalState := endState
    do
        rulesToGFG ()

    member this.PrintToDot output =
        let head = "digraph GFG { \n"
        let edges = this.Edges
                    |> Seq.map (fun e -> sprintf 
                                             "%i -> %i [label=\"%s\"]; \n" 
                                             e.Source 
                                             e.Target 
                                             (e.Tag.ToString()))
                    |> Seq.toList            
        System.IO.File.WriteAllLines (output, ([head] @ edges @ ["}"]))