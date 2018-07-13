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

type AutomatonPart = {starts: AutomatonEdge list; ends: AutomatonEdge list}

[<AllowNullLiteral>]
type Automaton(id: int, name: string, start: AutomatonNode, final: AutomatonNode) =
    member this.Id = id
    member this.Name = name
    member this.Start = start
    member this.Final = final

let createEdge label' =
    {label = label'; dir = null}
    
type AutomataFactory() = 
    let mutable nodes = new ResizeArray<AutomatonNode>()
    let mutable nonterminals = new ResizeArray<Automaton>()
    let mutable alphabet = new HashSet<EdgeSymbol>()
    let mutable startNonterminalId = 0

    let nonterminalIds = new Dictionary<string, int * int>()

    member this.Terminal name = 
        let token = Term name
        let edge = createEdge token 
        alphabet.Add token |> ignore
        {starts = [edge]; ends = [edge]}
    
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
        {starts = [edge]; ends = [edge]}
   
    member this.Epsilon() =
        let token = Epsilon()
        let edge = createEdge token
        alphabet.Add token |> ignore
        {starts = [edge]; ends = [edge]}
    
    member this.Sequence left right =
        let node = AutomatonNode(nodes.Count, right.starts)
        nodes.Add node
        
        left.ends |> List.iter (fun e -> e.dir <- node)

        {starts = left.starts; ends = right.ends}
       
    member this.Alternation left right =
        //TODO: left <-> right?
        {starts = left.starts @ right.starts; ends = left.ends @ right.ends}
    
    member this.Rule name (part: AutomatonPart) =
        if (nonterminalIds.ContainsKey name) then
            let nontId, nodeId = nonterminalIds.Item name

            let start = AutomatonNode(nodeId, part.starts)
            let final = AutomatonNode(nodes.Count, [])
            nodes.Add final

            part.ends |> List.iter (fun e -> e.dir <- final)

            let nonterminal = new Automaton(nontId, name, start, final)

            nonterminals.[nontId] <- nonterminal
            nodes.[nodeId] <- start
            nonterminal
        else
            let start = AutomatonNode(nodes.Count, part.starts)
            nodes.Add start 
            let final = AutomatonNode(nodes.Count, [])
            nodes.Add final

            part.ends |> List.iter (fun e -> e.dir <- final)

            let nonterminal = new Automaton(nonterminals.Count, name, start, final)
            nonterminalIds.Add(name, (nonterminals.Count, start.Id))
            nonterminals.Add nonterminal
            nonterminal
    
    member this.Start name (part: AutomatonPart) = 
        let rule = this.Rule name part
        startNonterminalId <- rule.Id
        rule
    
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
        let mutable fsaStarts = []
        let fsaFinals = new HashSet<int<positionInGrammar>>()

        nonterminals |> Seq.iter (fun n -> 
            fsaStateNames.Add(n.Start.Id * 1<positionInGrammar>, n.Name)
            fsaStarts <- (HashSet [n.Start.Id * 1<positionInGrammar>]) :: fsaStarts
            fsaFinals.Add (n.Final.Id * 1<positionInGrammar>) |> ignore)

        let fsaStartNumber = startNonterminalId

        let internalFSA: InternalFSA =
            {States = fsaStates; 
             Alphabet = fsaAlphabet; 
             StateToNontermName = fsaStateNames;
             StartComponentNumber = fsaStartNumber;
             StartStates = fsaStarts |> List.toArray;
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
