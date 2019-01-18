﻿module BioParser

open System.Collections.Generic

open YC.API
open YC.Frontends.YardFrontend
open YC.Core.Conversions.ExpandMeta
open YC.Parsing.Common.GraphInput
open YC.Core.Conversions
open YC.Core.IL
open YC.Core
//open Yard.Generators.YardPrinter

//open MathNet.Numerics.LinearAlgebra.Double

open MatrixKernels
open GraphParsing
open MySparseGraphParsingImpl
open SparseGraphParsingImpl
open YC.Core.Conversions.CNFandBNF

type ParsingResult = 
    | CPU of Dictionary<Util.NonTerminal, MySparseMatrix> //System.Collections.BitArray> 
    | GPU of Dictionary<Util.NonTerminal, MySparseMatrix>

type BioParser(grammar) =
    let (conversions: Conversion list) =
        [new ExpandEbnfStrict.ExpandEbnf(); new ExpandRepet.ExpandExpand(); 
         new ExpandMeta.ExpandMeta(); new ExpandInnerAlt.ExpandInnerAlt();
         new ExpandTopLevelAlt.ExpandTopLevelAlt(); new CNF()]
    let yardFE = new YardFrontend()
    let IL = yardFE.ParseGrammar grammar
    let finalIL = 
        let converted = 
            conversions |> List.fold (fun il conv -> conv.ConvertGrammar il) IL.grammar
        in {IL.Definition.info = {fileName = ""}; head = None; foot = None; grammar = converted; 
            options = Map.empty; tokens = Map.empty}
    let startN = (finalIL.grammar.Head.rules |> List.find (fun r -> r.isStart)).name.text

    let tokenizer x =
        match x with
        | "A" -> 1<token>
        | "C" -> 2<token>
        | "G" -> 3<token>
        | "T" | "U" -> 4<token>
        | _ -> 5<token>

    let buildInputGraph (input: string) =
        let edges = 
            Array.init (input.Length + 1) id
            |> Array.pairwise
            |> Array.mapi (fun i (s, e) -> new ParserEdge<_>(s, e, tokenizer (string input.[i])))
        let graph = SimpleInputGraph<_>([|0|], [|input.Length|], id)
        graph.AddVerticesAndEdgeRange edges |> ignore
        graph
    
    member private this.parse<'MatrixType> handler input =
        graphParseGPU  (buildInputGraph input) finalIL tokenizer
        //graphParse<'MatrixType, _> (buildInputGraph input) handler finalIL tokenizer 1
    
    member val StartNonTerm = startN with get

//    member this.PrintGrammarCNF() =
//        let printer = new YardPrinter()
//        printer.Generate(finalIL, false).ToString()

    member this.Parse isGpu (input: string) =
        if isGpu
        then
            let dict, _, _, _ = 
                this.parse<MySparseMatrix> (new MySparseHandler(input.Length + 1)) input
                //this.parse<System.Collections.BitArray> (new DenseBitMatrix.DenseBitHandler(input.Length + 1)) input
            in GPU dict
        else 
            let dict, _, _, _ = 
                this.parse<MySparseMatrix> (new MySparseHandler(input.Length + 1)) input
                //this.parse<System.Collections.BitArray> (new DenseBitMatrix.DenseBitHandler(input.Length + 1)) input
            in GPU dict
                //this.parse<SparseMatrix> (new SparseHandler(input.Length + 1)) input
                //this.parse<System.Collections.BitArray> (new DenseBitMatrix.DenseBitHandler(input.Length + 1)) input
            //in CPU dict 