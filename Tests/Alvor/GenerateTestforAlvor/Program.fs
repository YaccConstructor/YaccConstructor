module PrintTest

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Graphviz
open Microsoft.FSharp.Text

let baseInputGraphsPath = "../../../Tests/AbstractPerformance/TSQL"  

let path baseInputGraphsPath name = System.IO.Path.Combine(baseInputGraphsPath,name)

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let loadDotToQG baseInputGraphsPath gFile =
    let g = loadGraphFromDOT(path baseInputGraphsPath gFile)
    let qGraph = new AdjacencyGraph<int, TaggedEdge<_,string>>()
    g.Edges 
    |> Seq.iter(
        fun e -> 
            let edg = e :?> DotEdge<string>
            qGraph.AddVertex(int edg.Source.Id) |> ignore
            qGraph.AddVertex(int edg.Destination.Id) |> ignore
            qGraph.AddEdge(new TaggedEdge<_,_>(int edg.Source.Id,int edg.Destination.Id,edg.Label)) |> ignore)
    qGraph

let loadInputGraph gFile =
    let qGraph = loadDotToQG baseInputGraphsPath gFile
    qGraph

let resultDirectoryPath = ref @"../../result"

let getResultFileName path pref =
    // 1) для получения имени файла есть  System.IO.Path.GetFileName
    // 2) Собирать путь лучше через System.IO.Path.Combine Он сам отследит все слеши на границах и т.д.
    System.IO.Path.Combine (!resultDirectoryPath, pref + System.IO.Path.GetFileName path)
                                  //path.Substring(path.LastIndexOf("\\") + 1)

let PrintGraph (srcFilePath:string) =
    if not <| System.IO.Directory.Exists(!resultDirectoryPath)
    then System.IO.Directory.CreateDirectory(!resultDirectoryPath) |> ignore
    let outFile = System.IO.Path.Combine(!resultDirectoryPath,(System.IO.Path.GetFileNameWithoutExtension(srcFilePath)))
    let res = ref ""
    let print srcFilePath =
        let InputGraph = loadInputGraph srcFilePath
        let Vertex = InputGraph.Vertices |> Array.ofSeq
        let countVert = Vertex.Length
        //for k in 1..countVert - 3 do //for break literals
        for k in 0..countVert - 1 do  //for literals without break
            let outEdges = InputGraph.OutEdges(Vertex.[k]) |> Array.ofSeq
            let count = outEdges.Length
            if count = 1 then res := !res + "\"" + outEdges.[0].Tag + "\" "
            if count > 1 then
                res := !res  + "{ "
                for i in 0..count-2 do
                    res := !res + "\"" + outEdges.[i].Tag + "\"" + ", "
                res := !res + "\"" + outEdges.[count-1].Tag + "\"" + " } "
    print srcFilePath 
    File.WriteAllText(outFile,!res)
    
   
                    
                   
let PrintGraphAllDirectory (directoryName:string) =
    resultDirectoryPath := System.IO.Path.Combine(directoryName, "results")
         //directoryName + @"\results\"
    System.IO.Directory.GetFiles(directoryName,"*.dot")
    |> Array.iter PrintGraph

do 
    let inPath = ref @"..\..\tests\s2.dot"
    let lexerDir = ref false
    let commandLineSpecs =
        [
         "-f", ArgType.String (fun s -> inPath := path baseInputGraphsPath s), "Input file."
         "-d", ArgType.String (fun s -> lexerDir := true; inPath := s), "Input dir. Use for parse all files in specified directory."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs

    !inPath
    |> if !lexerDir
       then PrintGraphAllDirectory
       else PrintGraph 