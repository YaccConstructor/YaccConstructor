module BioParser

open System.Collections.Generic

open YC.API
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta
open AbstractAnalysis.Common
open Yard.Core.Conversions
open Yard.Core.IL.Definition
open Yard.Core
open Yard.Generators.YardPrinter

open MathNet.Numerics.LinearAlgebra.Double

open MatrixKernels
open GraphParsing
open MySparseGraphParsingImpl
open SparseGraphParsingImpl

type ParsingResult = 
    | CPU of Dictionary<Util.NonTerminal, SparseMatrix> //System.Collections.BitArray> 
    | GPU of Dictionary<Util.NonTerminal, MySparseMatrix>

type BioParser(grammar) =
    let (conversions: Conversion list) =
        [new ExpandEbnfStrict.ExpandEbnf(); new ExpandRepet.ExpandExpand(); 
         new ExpandMeta.ExpandMeta(); new ExpandInnerAlt.ExpandInnerAlt();
         new ExpandTopLevelAlt.ExpandTopLevelAlt(); new ToChomNormForm.ToChomNormForm()]
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
        | "A" -> 1
        | "C" -> 2
        | "G" -> 3
        | _ -> 4

    let buildInputGraph (input: string) =
        let edges = 
            Array.init (input.Length + 1) id
            |> Array.pairwise
            |> Array.mapi (fun i (s, e) -> new ParserEdge<_>(s, e, tokenizer (string input.[i])))
        let graph = SimpleInputGraph<_>([|0|], [|input.Length|], id)
        graph.AddVerticesAndEdgeRange edges |> ignore
        graph
    
    member private this.parse<'MatrixType> handler input =
        graphParse<'MatrixType, _> (buildInputGraph input) handler finalIL tokenizer 1
    
    member val StartNonTerm = startN with get

    member this.PrintGrammarCNF() =
        let printer = new YardPrinter()
        printer.Generate(finalIL, false).ToString()

    member this.Parse isGpu (input: string) =
        if isGpu
        then
            let dict, _, _, _ = 
                this.parse<MySparseMatrix> (new MySparseHandler(input.Length + 1)) input
                //this.parse<System.Collections.BitArray> (new DenseBitMatrix.DenseBitHandler(input.Length + 1)) input
            in GPU dict
        else 
            let dict, _, _, _ = 
                this.parse<SparseMatrix> (new SparseHandler(input.Length + 1)) input
                //this.parse<System.Collections.BitArray> (new DenseBitMatrix.DenseBitHandler(input.Length + 1)) input
            in CPU dict 