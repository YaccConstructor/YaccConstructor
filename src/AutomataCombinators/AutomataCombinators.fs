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
type AutomatonNonterminal(id: int, name: string, start: AutomatonNode, final: AutomatonNode) =
    member this.Id = id
    member this.Name = name
    member this.Start = start
    member this.Final = final

let createEdge label' =
    {label = label'; dir = null}
    
type AutomataFactory() = 
    let mutable nodesCounter = 0
    let mutable nodes: AutomatonNode list = []
    let mutable nonterminals = new ResizeArray<AutomatonNonterminal>()
    let mutable alphabet = new HashSet<EdgeSymbol>()
    let mutable startNonterminalId = 0

    let nonterminalIds = new Dictionary<string, int>()

    member this.Terminal name = 
        let token = Term name
        let edge = createEdge token 
        alphabet.Add token |> ignore
        {starts = [edge]; ends = [edge]}
    
    member this.Reference name = 
        let id = 
            if (nonterminalIds.ContainsKey name) then
                nonterminalIds.Item name
            else
                nonterminals.Add null
                let newId = nonterminals.Count - 1
                nonterminalIds.Add(name, newId)
                newId
        
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
        let node = AutomatonNode(nodesCounter, right.starts)
        nodesCounter <- nodesCounter + 1
        nodes <- node :: nodes
        
        left.ends |> List.iter (fun e -> e.dir <- node)

        {starts = left.starts; ends = right.ends}
       
    member this.Alternation left right =
        //TODO: left <-> right?
        {starts = left.starts @ right.starts; ends = left.ends @ right.ends}
    
    member this.Rule name (part: AutomatonPart) =
        let start = AutomatonNode(nodesCounter, part.starts)
        nodesCounter <- nodesCounter + 1
        let final = AutomatonNode(nodesCounter, [])
        nodesCounter <- nodesCounter + 1

        nodes <- start :: final :: nodes

        part.ends |> List.iter (fun e -> e.dir <- final)

        if (nonterminalIds.ContainsKey name) then
            let existingId = nonterminalIds.Item name
            let nonterminal = new AutomatonNonterminal(existingId, name, start, final)
            nonterminals.Item existingId <- nonterminal
            nonterminal
        else
            let nonterminal = new AutomatonNonterminal(nonterminals.Count, name, start, final)
            nonterminalIds.Add(name, nonterminals.Count)
            nonterminals.Add nonterminal
            nonterminal
    
    member this.Start name (part: AutomatonPart) = 
        let rule = this.Rule name part
        startNonterminalId <- rule.Id
        rule
    
    member this.Combinators =
        this.Terminal, this.Reference, this.Epsilon, this.Rule, this.Start, this.Sequence, this.Alternation

    member this.Produce() = 
        let fsaStates = Array.create nodes.Length Array.empty
        nodes |> List.iter (fun n -> 
            let transitions = (n.Transitions |> List.map (fun t -> 
                (t.label, t.dir.Id * 1<positionInGrammar>))) |> List.toArray
            Array.set fsaStates n.Id transitions)
        
        let fsaAlphabet = Set alphabet
        let fsaStateNames = new Dictionary<int<positionInGrammar>, string>()
        let mutable fsaStarts = []
        let fsaFinals = new HashSet<int<positionInGrammar>>()

        nonterminals |> Seq.iter (fun n -> 
            fsaStateNames.Add(n.Id * 1<positionInGrammar>, n.Name)
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
        
        new FSA(internalFSA)
