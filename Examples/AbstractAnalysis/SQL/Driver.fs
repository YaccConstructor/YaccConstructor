//  Driver.fs contains entry point of MS-SQL parser.
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module MSSqlParser

open AbstractParsing.Common
open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RNGLR.AST
open Yard.Examples.MSParser
open LexerHelper
open Yard.Utils.SourceText
open Yard.Utils.StructClass
open Yard.Utils.InfoClass
open System
open System.IO
open QuickGraph.Algorithms
open QuickGraph.Graphviz
open Graphviz4Net.Dot
open QuickGraph
open Graphviz4Net.Dot.AntlrParser

let lastTokenNum = ref 0L
let traceStep = 50000L

let loadGraphFromDOT filePath = 
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let baseInputGraphsPath = "../../tests"
let path name = System.IO.Path.Combine(baseInputGraphsPath,name)

let loadDotToQG gFile =
    let g = loadGraphFromDOT(path gFile)
    let qGraph = ParserInputGraph()
        //new AdjacencyGraph<int, TaggedEdge<_,_>>()
    let getTkn str =
        try
            Yard.Examples.MSParser.genLiteral str 0UL 0UL
        with 
        | _ -> IDENT(new SourceText(str, new SourceRange(0UL,0UL)))
    g.Edges 
    |> Seq.iter(
        fun e -> 
            let edg = e :?> DotEdge<string>
            qGraph.AddVertex(int edg.Source.Id) |> ignore
            qGraph.AddVertex(int edg.Destination.Id) |> ignore
            qGraph.AddEdge(new ParserEdge<_>(int edg.Source.Id,int edg.Destination.Id, getTkn edg.Label)) |> ignore)
    qGraph
//    let pGraph = 
//    pGraph.AddVerticesAndEdgeRange(qGraph.Edges)

let justParse (path:string) =
    use reader = new System.IO.StreamReader(path)
    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    let allTokens = seq []
    let res =
        let g = loadDotToQG "s1.dot"
        (new Yard.Generators.RNGLR.AbstractParser.Parser<_>()).Parse buildAstAbstract g
        //buildAstAbstract 
    //buildAst allTokens
    //printfn "Time for parse file %s = %A" path (System.DateTime.Now - start)
    res

let p = new ProjInfo()
let mutable counter = 1<id>

let Parse (srcFilePath:string) = 
    let StreamElement = new StreamReader(srcFilePath, System.Text.Encoding.UTF8)  
    let map = p.GetMap StreamElement
    //Array.iter (printfn "%A") map
    //Lexer.id <- counter
    p.AddLine counter map
    counter <- counter + 1<id>
    //Lexer.id <- from (ProjInfo)
    match justParse srcFilePath with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg, dbg) ->
        //let coordinates = 
//            let x,y = tokenPos tok
//            let x = p.GetCoordinates x
//            let y = p.GetCoordinates y
//            sprintf "(%A,%A) - (%A,%A)" x.Line x.Column y.Line y.Column
        let data =
            let d = tokenData tok
            if isLiteral tok then ""
            else (d :?> SourceText).text
        let name = tok |> tokenToNumber |> numToString
        printfn "Error in file %s on position  on Token %s %s: %s" srcFilePath name data msg
        //dbg.lastTokens(10) |> printfn "%A"
        dbg.drawGSSDot @"..\..\stack.dot"
    | Yard.Generators.RNGLR.Parser.Success ast ->
        let GC_Collect () = 
            GC.Collect()    
            GC.WaitForPendingFinalizers()
            GC.GetTotalMemory(true)
        GC_Collect() |> printfn "%A" 
        ast.collectWarnings (tokenPos >> fun (x,y) -> let x = RePack x in x.Line + 1<line> |> int, int x.Column)
        |> Seq.groupBy snd
        |> Seq.sortBy (fun (_,gv) -> - (Seq.length gv))
        |> Seq.iter (fun (prods, gv) -> 
            printfn "conf# %i  prods: %A" (Seq.length gv) prods
            gv |> (fun s -> if Seq.length s > 5 then Seq.take 5 s else s) |> Seq.map fst |> Seq.iter (printfn "    %A"))
        //defaultAstToDot ast @"..\..\ast.dot"
        //ast.ChooseLongestMatch()
        //let translated = translate translateArgs ast : list<Script>            
        //printfn "%A" translated
        //translated.Head  
        
let ParseAllDirectory (directoryName:string) =
    System.IO.Directory.GetFiles directoryName
    |> Array.iter Parse

do 
    let inPath = ref @"..\..\..\..\..\Tests\Materials\ms-sql\sysprocs\test.sql" 
    //let inPath = ref @"..\..\..\..\..\Tests\Materials\ms-sql\sysprocs\sp_addserver.sql"
    let parseDir = ref false
    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> inPath := s), "Input file."
         "-d", ArgType.String (fun s -> parseDir := true; inPath := s), "Input dir. Use for parse all files in specified directory."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs

    !inPath
    |> if !parseDir
       then ParseAllDirectory
       else Parse
    
//@"D:\projects\YC\recursive-ascent\Tests\Materials\ms-sql\sysprocs\sp_addserver.sql" 
//@"..\..\..\..\..\Tests\Materials\ms-sql\sqlsrvanalysissrvcs\MonitoringSSAS\config_data_server\get_query_text.sql"
//@"C:\Users\Anastasiya\Desktop\Projects\Reengineering\recursive-ascent\Tests\materials\ms-sql\sysprocs\sp_addserver.sql"