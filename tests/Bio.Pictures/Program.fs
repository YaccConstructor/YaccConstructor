// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Drawing
open System.IO
open System.Text

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

let grammar = @"../../../../src/YC.GrammarZOO/Bio/tests/bio_brackets.yrd"
let yardFE = new Yard.Frontends.YardFrontend.YardFrontend()
let IL = yardFE.ParseGrammar grammar

let tokenizer x =
    match x with
    | "A" -> 1
    | "C" -> 2
    | "G" -> 3
    | _ -> 4

let getData path = 
    let d = new System.Text.StringBuilder()
    let meta = ref ""
    let lst = new ResizeArray<_>()
    for s in System.IO.File.ReadAllLines(path) do
        if s.[0] = '>'
        then
            if !meta <> ""
            then 
                lst.Add(!meta, d.ToString())
                d.Clear() |> ignore
            meta := s
        else
            d.Append s |> ignore
    lst.Add(!meta, d.ToString())
    lst.ToArray()

let buildInputGraph (input: string) =
    let edges = 
        Array.init (input.Length + 1) id
        |> Array.pairwise
        |> Array.mapi (fun i (s, e) -> new ParserEdge<_>(s, e, tokenizer (string input.[i])))
    let graph = SimpleInputGraph<_>([|0|], [|input.Length|], id)
    graph.AddVerticesAndEdgeRange edges |> ignore
    graph

let parse<'MatrixType> handler input =
    graphParse<'MatrixType, float> input handler IL tokenizer 1
let parseCPU (graph: SimpleInputGraph<_>) = 
    parse<SparseMatrix> (new SparseHandler(graph.VertexCount)) graph
let parseGPU (graph: SimpleInputGraph<_>) =
    parse<MySparseMatrix> (new MySparseHandler(graph.VertexCount)) graph

let getPictureSource isGpu (graph: SimpleInputGraph<_>) =
    let dim = graph.VertexCount
    if isGpu 
    // because Sparse and MySparse have different parent classes
    then 
        let parsingMatrix, startN, _, _ = parseGPU graph
        parsingMatrix.[startN].ToArray()     
    else
        let parsingMatrix, startN, _, _ = parseCPU graph
        parsingMatrix.[startN].ToArray()

let drawPicture (matrix: float[,]) output = 
    let dim = Array2D.length1 matrix

    let bmp = new Bitmap(dim, dim)
    for i in 0 .. dim - 1 do
        for j in 0 .. dim - 1 do
                if matrix.[i, j] <> 0.0
                then bmp.SetPixel (i, j, Color.Black)
    bmp.Save(output)

let drawPositiveExamples isGpu fastaFile =
    let data = 
        getData fastaFile 
        |> Array.filter (fun (meta, _) -> meta.Contains("Bacteria;"))
        |> Array.map (fun (meta, gen) -> (meta.Split().[0].[1 ..], gen))
    let path = "../../positive/"
    Directory.CreateDirectory(path) |> ignore
    for (id, gen) in data do
        let matrix = getPictureSource isGpu (buildInputGraph gen)
        drawPicture matrix (path + id + ".bmp")

let drawNegativeExamples isGpu minLength maxLength fastaFiles =
    let remove16s (genome: string) toRemove =
        if Array.isEmpty toRemove
        then genome
        else
            let builder = new StringBuilder()
            let cur = ref 0
            toRemove
            |> Array.iter 
                   (fun (i, j) -> builder.Append genome.[!cur .. i] |> ignore; cur := j)
            builder.Append(genome.[!cur ..]).ToString()

    let path = "../../negative/"
    let random = System.Random()
    Directory.CreateDirectory(path) |> ignore
    for f in fastaFiles do
        let data = (getData f).[0]
        let metaParts = (fst data).Split(',') |> Array.map (fun s -> s.Trim())
        let id = metaParts.[0].[1 ..] 
        let intervals16s = 
            metaParts.[3].Split()
            |> Array.map (fun s -> let p = s.Split(':') in (int p.[0], int p.[1]))
        let filteredGen = remove16s (snd data) intervals16s
        for i in 0 .. 100 .. filteredGen.Length - maxLength - 1 do
            let length = random.Next(minLength, maxLength)
            let name = sprintf "%s_%i_%i.bmp" id i length
            let matrix = getPictureSource isGpu (buildInputGraph filteredGen.[i .. i + length - 2])
            drawPicture matrix (path + name)

[<EntryPoint>]
let main argv = 
    let genomeFiles = 
        Directory.GetFiles("../../../data/bio/complete_genome/", "*.txt", SearchOption.AllDirectories)
    drawPositiveExamples false "../../SILVA_128_SSURef_Nr99_tax_silva_first_500k_lines.fasta"
    drawNegativeExamples false 500 700 genomeFiles
    0
