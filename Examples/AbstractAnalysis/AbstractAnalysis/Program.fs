module AbstractFsLex.Test

open Graphviz4Net.Dot.AntlrParser
open System.IO
open Graphviz4Net.Dot
open QuickGraph
open NUnit.Framework
open AbstractLexer.Common
open AbstractLexer.Core
open QuickGraph.Algorithms
open Calc.AbstractParser
open QuickGraph.Algorithms
open QuickGraph.Graphviz

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../../AbstractAnalysisTests"
   
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
    for e in qGraph.Edges do lexerInputG.AddEdgeForsed (new AEdge<_,_>(e.Source,e.Target,(Some e.Tag, Some (e.Tag+"|"))))
    lexerInputG

let printG res (fName:string) =
    let f = GraphvizAlgorithm(res)
    let printEdg (e:AbstractParsing.Common.ParserEdge<_>) =
        let printBrs brs =
            "["
            + (brs |> Array.map (function Some x -> string x | None -> "None") |> String.concat "; ")
            + "]"
        match e.Tag with
        | NUMBER(v,br) -> "NUM: " + v + "; br= " + printBrs br
        | PLUS(v,br)   
        | MULT(v,br)   
        | RBRACE(v,br)
        | POW(v,br)
        | DIV(v,br)
        | LBRACE(v,br) ->  v + "; br= " + printBrs br
        | e -> string e 
    f.FormatEdge.Add(fun e -> (e.EdgeFormatter.Label.Value <- printEdg e.Edge))
    let str = f.Generate()
    let c = System.IO.Path.GetInvalidFileNameChars()
    let fName1 = c |> Array.fold (
                                    fun (name:string) ch -> name.Replace(ch,'_')) fName
    System.IO.File.WriteAllText(fName1 + ".lexer.dot" ,str)

let go file =
    let lexerInputGraph = loadLexerInputGraph file
    let res = Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)
    printG res file
    let r = (new Yard.Generators.RNGLR.AbstractParser.Parser<_>()).Parse  buildAstAbstract res
    printfn "%A" r
    match r with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, message, debug) ->
        printfn "Error in position %d on Token %A: %s" num tok message
        debug.drawGSSDot (file + ".out.dot")
    | Yard.Generators.RNGLR.Parser.Success tree ->
        tree.PrintAst()
        defaultAstToDot tree (file + ".ast.dot")

let file = ref "simple_sum.dot"
let commandLineSpecs =
    ["-i", ArgType.String (fun s -> file := s), "Input file."]
    |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))

ArgParser.Parse commandLineSpecs
go !file
