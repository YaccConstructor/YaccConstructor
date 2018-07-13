module Yard.Generators.Common.AutomataCombinators

open Yard.Generators.Common.FSA.Common
open Yard.Generators.Common.FSA
open AbstractAnalysis.Common

open System.Collections.Generic

[<AllowNullLiteral>]
type AutomatonNode(id: int, transitions: AutomatonEdge list) =
    member this.Id = id
    member this.Transitions = transitions

and  AutomatonEdge = {label: EdgeSymbol; mutable dir: AutomatonNode}

type AutomatonPart = {starts: AutomatonEdge list; ends: AutomatonEdge list; epsilon: bool}

[<AllowNullLiteral>]
type Automaton(id: int, name: string, start: AutomatonNode, final: AutomatonNode) =
    member this.Id = id
    member this.Name = name
    member this.Start = start
    member this.Final = final

let createEdge label' =
    {label = label'; dir = null}
    
type AutomataFactory() = 
    let nodes = new ResizeArray<AutomatonNode>([new AutomatonNode(0, [])])
    let lastNode = nodes.[0]
    let finals = new HashSet<AutomatonNode>([lastNode])

    let nonterminals = new ResizeArray<Automaton>()
    let alphabet = new HashSet<EdgeSymbol>()
    let mutable startNonterminalId = 0

    let nonterminalIds = new Dictionary<string, int * int>()

    let createRule name (part: AutomatonPart) =
        if (nonterminalIds.ContainsKey name) then
            let nontId, nodeId = nonterminalIds.Item name
            let start = AutomatonNode(nodeId, part.starts)
            if (part.epsilon) then
                finals.Add start |> ignore

            part.ends |> List.iter (fun e -> e.dir <- lastNode)

            let nonterminal = new Automaton(nontId, name, start, lastNode)

            nonterminals.[nontId] <- nonterminal
            nodes.[nodeId] <- start
            nonterminal
        else
            let start = AutomatonNode(nodes.Count, part.starts)
            nodes.Add start 
            if (part.epsilon) then
                finals.Add start |> ignore

            part.ends |> List.iter (fun e -> e.dir <- lastNode)

            let nonterminal = new Automaton(nonterminals.Count, name, start, lastNode)
            nonterminalIds.Add(name, (nonterminals.Count, start.Id))
            nonterminals.Add nonterminal
            nonterminal

    member this.Terminal name = 
        let token = Term name
        let edge = createEdge token 
        alphabet.Add token |> ignore
        {starts = [edge]; ends = [edge]; epsilon = false}
    
    member this.Reference name = 
        let id = 
            if (nonterminalIds.ContainsKey name) then
                nonterminalIds.Item name |> snd
            else
                let nontId = nonterminals.Count
                let nodeId = nodes.Count

                nonterminals.Add null
                nodes.Add null
                nonterminalIds.Add(name, (nontId, nodeId))
                nodeId
        
        let token = Nonterm (id * 1<positionInGrammar>)
        let edge = createEdge token
        alphabet.Add token |> ignore
        {starts = [edge]; ends = [edge]; epsilon = false;}
   
    member this.Epsilon =
        {starts = []; ends = []; epsilon = true}
    
    member this.Sequence left right =
        let node = AutomatonNode(nodes.Count, right.starts)
        nodes.Add node

        if (right.epsilon) then
            finals.Add node |> ignore
        
        left.ends |> List.iter (fun e -> e.dir <- node)

        {starts = left.starts; ends = right.ends; epsilon = false}
       
    member this.Alternation left right =
        //TODO: left <-> right?
        {starts = right.starts @ left.starts; 
         ends = right.ends @ left.ends; 
         epsilon = left.epsilon || right.epsilon}

    member this.Rule name (part: AutomatonPart) =
        createRule name part |> ignore
    
    member this.Start name (part: AutomatonPart) = 
        let rule = createRule name part
        startNonterminalId <- rule.Id
    
    member this.Combinators =
        this.Terminal, this.Reference, this.Epsilon, this.Rule, this.Start, this.Sequence, this.Alternation

    member this.Produce() = 
        let fsaStates = Array.create nodes.Count Array.empty
        nodes |> Seq.iter (fun n -> 
            let transitions = (n.Transitions |> List.map (fun t -> 
                (t.label, t.dir.Id * 1<positionInGrammar>))) |> List.toArray
            Array.set fsaStates n.Id transitions)
        
        let fsaAlphabet = Set alphabet
        let fsaStateNames = new Dictionary<int<positionInGrammar>, string>()
        let fsaStarts = Array.create nonterminals.Count null
        let fsaFinals = new HashSet<int<positionInGrammar>>(finals |> Seq.map (fun n -> n.Id * 1<positionInGrammar>))

        nonterminals |> Seq.iter (fun n -> 
            fsaStateNames.Add(n.Start.Id * 1<positionInGrammar>, n.Name)
            Array.set fsaStarts n.Id (HashSet [n.Start.Id * 1<positionInGrammar>])
            fsaFinals.Add (n.Final.Id * 1<positionInGrammar>) |> ignore)

        let fsaStartNumber = startNonterminalId

        let internalFSA: InternalFSA =
            {States = fsaStates; 
             Alphabet = fsaAlphabet; 
             StateToNontermName = fsaStateNames;
             StartComponentNumber = fsaStartNumber;
             StartStates = fsaStarts;
             FinalStates = fsaFinals}
        
        let printToken token =
            match token with
            | Term t -> t
            | Nonterm i -> fsaStateNames.Item i
            | Epsilon() -> ""

        let tokens = Map (alphabet 
            |> Seq.filter (fun s -> 
                match s with
                | Term _ -> true
                | Nonterm _ -> false
                | Epsilon() -> false)
            |> Seq.map (fun s -> (printToken s, None)))
        
        new FSA(internalFSA), tokens
