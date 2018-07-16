module Yard.Generators.Common.AutomataCombinators

open Yard.Generators.Common.FSA.Common
open Yard.Generators.Common.FSA
open AbstractAnalysis.Common

open System.Collections.Generic

type private AutomatonNodeId = int<positionInGrammar>
let private nid i = i * 1<positionInGrammar>

type AutomatonEdge = { label: EdgeSymbol; mutable dir: AutomatonNodeId }
type private AutomatonNode = AutomatonEdge list 

type AutomatonPart = { starts: AutomatonEdge list; ends: AutomatonEdge list; epsilon: bool }

let private createEdge label' = { label = label'; dir = nid -1 }
    
type AutomataFactory() = 
    let nodes = new ResizeArray<AutomatonNode> ([List.empty])
    let lastNodeId = nid 0
    let finals = new HashSet<AutomatonNodeId> ([lastNodeId])

    let alphabet = new HashSet<EdgeSymbol>()
    
    let nonterminalStateNames = new Dictionary<AutomatonNodeId, string>()
    let nonterminalIds = new Dictionary<string, int * AutomatonNodeId>()
    let mutable nonterminalsCounter = 0
    let mutable startNonterminalId = 0

    let nextNonterminalId() = 
        let prev = nonterminalsCounter
        nonterminalsCounter <- nonterminalsCounter + 1
        prev

    let createRule name (part: AutomatonPart) =
        let startNode: AutomatonNode = part.starts
        part.ends |> List.iter (fun e -> e.dir <- lastNodeId)

        if (nonterminalIds.ContainsKey name) then
            let nontId, nodeId = nonterminalIds.[name]

            if (part.epsilon) then
                finals.Add nodeId |> ignore

            nodes.[int nodeId] <- startNode
            nontId
        else
            let startNodeId = nid nodes.Count
            nodes.Add startNode 

            if (part.epsilon) then
                finals.Add startNodeId |> ignore

            let nontId = nextNonterminalId()
            nonterminalIds.Add (name, (nontId, startNodeId))
            nonterminalStateNames.Add (startNodeId, name)
            nontId
    
    member this.TerminalToken name =
        let token = Term name
        alphabet.Add token |> ignore
        token
    
    member this.NonterminalToken name =
        let id = 
            if (nonterminalIds.ContainsKey name) then
                nonterminalIds.Item name |> snd
            else
                let nontId = nextNonterminalId()
                let nodeId = nid nodes.Count

                nodes.Add []
                nonterminalIds.Add(name, (nontId, nodeId))
                nonterminalStateNames.Add(nodeId, name)
                nodeId
        
        let token = Nonterm (id)
        alphabet.Add token |> ignore
        token

    member this.Terminal token = 
        let edge = createEdge token 
        {starts = [edge]; ends = [edge]; epsilon = false}
    
    member this.Reference token = 
        let edge = createEdge token
        {starts = [edge]; ends = [edge]; epsilon = false;}
   
    member this.Epsilon =
        {starts = []; ends = []; epsilon = true}
    
    member this.Sequence left right =
        let node = right.starts
        let nodeId = nid nodes.Count
        nodes.Add node

        if right.epsilon then
            finals.Add nodeId |> ignore
        
        left.ends |> List.iter (fun e -> e.dir <- nodeId)

        {starts = left.starts; ends = right.ends; epsilon = false}
       
    member this.Alternation left right =
        {starts  = right.starts @ left.starts;
         ends    = right.ends @ left.ends; 
         epsilon = left.epsilon || right.epsilon}
    
    member this.Alternations (parts: AutomatonPart list) = 
        List.fold this.Alternation parts.Head parts.Tail
        (*{starts = parts |> List.collect (fun p -> p.starts);
         ends = parts |> List.collect (fun p -> p.ends);
         epsilon = parts |> List.exists (fun p -> p.epsilon)}*)

    member this.Rule name (part: AutomatonPart) =
        createRule name part |> ignore
    
    member this.Start name (part: AutomatonPart) = 
        let ruleId = createRule name part
        startNonterminalId <- ruleId
    
    member this.Combinators =
        this.TerminalToken, 
        this.NonterminalToken, 
        this.Terminal, 
        this.Reference, 
        this.Epsilon, 
        this.Rule, 
        this.Start, 
        this.Sequence, 
        this.Alternation

    member this.Produce() = 
        let fsaStates = Array.zeroCreate nodes.Count

        nodes |> Seq.iteri 
            (
                fun i n -> 
                    let transitions = n 
                                      |> List.map (fun t -> (t.label, t.dir))
                                      |> List.toArray

                    Array.set fsaStates i transitions
            )
        
        let fsaStarts = Array.zeroCreate nonterminalIds.Count

        nonterminalIds.Values |> Seq.iter 
            (
                fun (nontId, nodeId) -> 
                    Array.set fsaStarts nontId (HashSet [nodeId])
            )
            

        let internalFSA: InternalFSA =
            {
                States = fsaStates
                Alphabet = set alphabet
                StateToNontermName = nonterminalStateNames
                StartComponentNumber = startNonterminalId
                StartStatesOfEachNonterminal = fsaStarts
                FinalStates = finals
            }
        
        let printToken token =
            match token with
            | Term t -> t
            | Nonterm i -> nonterminalStateNames.[i]
            | Epsilon() -> ""
        
        new FSA(internalFSA)
