// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Drawing
open System.IO

open YC.API
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions.ExpandMeta
open AbstractAnalysis.Common

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

let drawPicture (inputGraph: SimpleInputGraph<_>) output = 
    let dim = inputGraph.VertexCount
    
    let handler = new MySparseHandler(dim)
    let parsingMatrix, startN, _, _ = 
        graphParse<MySparseMatrix, float> inputGraph handler IL tokenizer 1
    
    let startMatrix = parsingMatrix.[startN]
    let bmp = new Bitmap(dim, dim)
    for i in 0 .. dim - 1 do
        for j in 0 .. dim - 1 do
                if startMatrix.GetItem(i, j) <> 0.0 
                then bmp.SetPixel (i, j, Color.Black) 
//                else bmp.SetPixel (i, j, Color.White) 
    bmp.Save(output)

let drawPositiveExamples fastaFile =
    let data = 
        getData fastaFile 
        |> Array.filter (fun (meta, _) -> meta.Contains("Bacteria;"))
        |> Array.map (fun (meta, gen) -> (meta.Split().[0].[1 ..], gen))
    let path = "../../positive/"
    Directory.CreateDirectory(path) |> ignore
    for (id, gen) in data do
        drawPicture (buildInputGraph gen) (path + id + ".bmp")

[<EntryPoint>]
let main argv = 
    drawPositiveExamples "../../SILVA_128_SSURef_Nr99_tax_silva_first_500k_lines.fasta"
    0
