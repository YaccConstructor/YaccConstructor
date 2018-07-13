module Shuffle.Parsing

open System.IO
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.Common.ASTGLL
open Yard.Generators.Common.ASTGLLFSA
open Yard.Generators.GLL.ParserCommon
open YC.API
open Yard.Frontends.YardFrontend
open Yard.Generators.GLL
open Yard.Core.Conversions.ExpandMeta

open System.Collections.Generic
open System.Linq
open System
open System.Threading

open TrieToFormula

open QuickGraph.Graphviz

open YC.GLL.SPPF

let getParserSource grammarFile conv = 
    let fe = new YardFrontend()
    let gen = new GLL()
    generate ((*grammarsDir + *)grammarFile)
             fe gen 
             None
             conv
             [|""|]
             [] :?> ParserSourceGLL

let graphEdgesForLinearInput (input : string[]) =
    let edges = new List<ParserEdge<_>>()
    for i in 0..(input.Length-1) do
        for start in 0..i do
            edges.Add ( new ParserEdge<_>(start, i + 1, input.[i]) )
    [|0|], [|for i in 1..input.Length -> i|], edges
        
         

let parseShuffledGrammarsLinearInput (grammars : string []) (input : string[]) =     
    let parserSources : ParserSourceGLL [] = 
        let conv = [new ExpandMeta()]
        grammars
        |> Array.map (fun grammarFile -> getParserSource grammarFile conv)
    let timeInit = System.DateTime.UtcNow;
    let parserInputPairs = 
        parserSources
        |> Array.map (fun ps ->
            let startVerts, finalVerts, edges = graphEdgesForLinearInput input
            let graph = new SimpleInputGraph<string>(startVerts, finalVerts, ps.StringToToken)
            graph.AddVerticesAndEdgeRange edges |> ignore
            ps,graph)
    
    let allRoots = 
        parserInputPairs
        |> Array.map (fun (ps, graph) ->
            getAllSPPFRoots ps graph, ps
            )
            
    let SPPFsFormulas = 
        allRoots
        |> Array.mapi (fun i (roots, ps) ->
            sppfRootsToFormula roots ps.IntToString i
            )
        
    let edgesMappingFormula = 
        linearInputToFormula input SPPFsFormulas.Length

    let finalFormula =        
        [|AND SPPFsFormulas; edgesMappingFormula |] |> AND |> reduceFormula
    
    System.IO.File.WriteAllText("formula.txt", finalFormula.ToString())
    
    let isParsed = Z3logic.solveFormula finalFormula
    printfn "Time: %A" (System.DateTime.UtcNow - timeInit)
    isParsed