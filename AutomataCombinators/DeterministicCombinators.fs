module AutomataCombinators

open Yard.Generators.Common.FSA.Common
open Yard.Generators.Common.FSA
open AbstractAnalysis.Common

open System.Collections.Generic

[<AllowNullLiteral>]
type AutomataNode(id: int, transitions: AutomataEdge list) =
    member this.Id = id
    member this.Transitions = transitions

and  AutomataEdge = {label: EdgeSymbol; mutable dir: AutomataNode}

type AutomataPart = {begins: AutomataEdge list; ends: AutomataEdge list}

type AutomataNonterminal = {id: int; name: string; start: AutomataNode; final: AutomataNode}

let createEdge label' =
    {label = label'; dir = null}
    
type AutomataFactory() = 
    let mutable nodesCounter = 0
    let mutable nonterminalsCounter = 0
    let mutable nodes: AutomataNode list = []
    let mutable nonterminals: AutomataNonterminal list = []
    let mutable alphabet: EdgeSymbol list = []

    member this.Token name = 
        let term = Term name
        let edge = createEdge term 
        alphabet <- term :: alphabet
        {begins = [edge]; ends = [edge]}
    
    member this.Rule name (part: AutomataPart) =
        let start = AutomataNode(nodesCounter, part.begins)
        nodesCounter <- nodesCounter + 1
        let final = AutomataNode(nodesCounter, [])
        nodesCounter <- nodesCounter + 1

        part.ends |> List.iter (fun e -> e.dir <- final)

        nodes <- start :: final :: nodes

        let nonterminal = {id = nonterminalsCounter; name = name; start = start; final = final}
        nonterminalsCounter <- nonterminalsCounter + 1
        nonterminals <- nonterminal :: nonterminals
        nonterminal

    member this.Produce = 
        let fsaStates = nodes |> List.map (fun n -> 
            (n.Transitions |> List.map (fun t -> 
                (t.label, t.dir.Id * 1<positionInGrammar>))) |> List.toArray) |> List.toArray
        
        let fsaAlphabet = Set alphabet
        let fsaNonterminalNames = new Dictionary<int<positionInGrammar>, string>()
        let mutable fsaStarts = []
        let fsaFinals = new HashSet<int<positionInGrammar>>()

        nonterminals |> List.iter (fun n -> 
            fsaNonterminalNames.Add(n.id * 1<positionInGrammar>, n.name)
            fsaStarts <- (HashSet [n.start.Id * 1<positionInGrammar>]) :: fsaStarts
            fsaFinals.Add (n.final.Id * 1<positionInGrammar>) |> ignore)

        let fsaStartsNumber = nonterminals.Length

        let internalFSA: InternalFSA =
            {States = fsaStates; 
             Alphabet = fsaAlphabet; 
             StateToNontermName = fsaNonterminalNames;
             StartComponentNumber = fsaStartsNumber;
             StartStates = fsaStarts |> List.toArray;
             FinalStates = fsaFinals}
        
        new FSA(internalFSA)
