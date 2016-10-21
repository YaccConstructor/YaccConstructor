namespace Yard.Generators.Common.GrammarFlowGraph

open System.Collections.Generic

open Yard.Core.IL
open Yard.Core.IL.Production
open QuickGraph

type GFGEdgeTag<'token> =
    | Entry
    | End
    | Call of int   // stores matching return node
    | Scan of 'token
    | Eps
    override this.ToString() =
        match this with
        | Entry -> "entry"
        | End -> "end"
        | Call n -> string n
        | Scan s -> s.ToString()
        | Eps -> ""

type GFGEdge<'token>(s, e, t)=
    inherit TaggedEdge<int, GFGEdgeTag<'token>>(s, e, t)

type GrammarFlowGraph<'token> (ruleList : Rule.t<Source.t,Source.t> list, mapToToken, EOF) as this =   // temporary solution
    inherit AdjacencyGraph<int, GFGEdge<'token>>()

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
        this.AddEdge (new GFGEdge<_>(source, target, tag)) |> ignore

    let newEdges isTerm source (target : int option) (s: Source.t) =
        let final = ref dummyState
        if target.IsNone then final := newState() else final := target.Value
        if isTerm
        then addEdge source !final (Scan (mapToToken s.text))   // temporary solution
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
            if List.isEmpty seq
            then
                let epsState = newState()
                addEdge startState epsState Entry
                addEdge epsState endState.Value End
                epsState
            else
                let entryState, exitState = newState(), newState()
                addEdge startState entryState Entry
                addEdge exitState endState.Value End
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
        let finalEdge = this.Edges |> Seq.find (fun e -> e.Target = !finalState)
        let src = finalEdge.Source
        this.RemoveEdge finalEdge |> ignore
        this.AddEdge (new GFGEdge<_>(src, !finalState, Scan EOF)) |> ignore
    do
        rulesToGFG ()
    
    member this.InitState = !initState
    member this.FinalState = !finalState

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