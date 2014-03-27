module TSQLTests

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractLexer.Common
open AbstractLexer.Core
open QuickGraph.Algorithms
open QuickGraph.Graphviz
open Microsoft.FSharp.Text
open YC.ReSharper.AbstractAnalysis.Languages.TSQL


let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../Tests/AbstractPerformance/TSQL"
let path name = System.IO.Path.Combine(baseInputGraphsPath,name)

let loadDotToQG gFile =
    let g = loadGraphFromDOT(path gFile)
    let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
    g.Edges 
    |> Seq.iter(
        fun e -> 
            let edg = e :?> DotEdge<string>
            qGraph.AddVertex(int edg.Source.Id) |> ignore
            qGraph.AddVertex(int edg.Destination.Id) |> ignore
            qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
    qGraph

let loadLexerInputGraph gFile =
    let qGraph = loadDotToQG gFile
    let lexerInputG = new LexerInputGraph<_>()
    lexerInputG.StartVertex <- 0
    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new LexerEdge<_,_>(e.Source,e.Target,Some(e.Tag, null)))
    lexerInputG

let resultDirectoryPath = ref @"../../result"

let getResultFileName path pref =
    // 1) для получения имени файла есть  System.IO.Path.GetFileName
    // 2) Собирать путь лучше через System.IO.Path.Combine Он сам отследит все слеши на границах и т.д.
    System.IO.Path.Combine (!resultDirectoryPath, pref + System.IO.Path.GetFileName path)
                                  //path.Substring(path.LastIndexOf("\\") + 1)

//let flg = ref false
let LexerTSQL (srcFilePath:string) =
    let tokenize srcFilePath = 
        let lexerInputGraph = loadLexerInputGraph srcFilePath
        let start = System.DateTime.Now
        for i in 1..10 do
            tokenize lexerInputGraph |> ignore
        System.GC.Collect()
        printf  "%s " (System.IO.Path.GetFileNameWithoutExtension(srcFilePath))
        printf " %A " <| (System.DateTime.Now - start).TotalMilliseconds / 10.0
        printfn " "
    tokenize srcFilePath

let LexerTSQLAllDirectory (directoryName:string) =
    resultDirectoryPath := System.IO.Path.Combine(directoryName, "results")
        // directoryName + @"\results\"
    System.IO.Directory.GetFiles(directoryName,"*.dot")
    |> Array.iter LexerTSQL

do 
    let inPath = ref @"..\..\tests\s2.dot"
    let lexerDir = ref false
    let commandLineSpecs =
        [
         "-f", ArgType.String (fun s -> inPath := path s), "Input file."
         "-d", ArgType.String (fun s -> lexerDir := true; inPath := s), "Input dir. Use for parse all files in specified directory."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs

    !inPath
    |> if !lexerDir
       then LexerTSQLAllDirectory
       else LexerTSQL

