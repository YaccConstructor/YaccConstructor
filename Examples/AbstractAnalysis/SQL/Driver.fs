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
    let g = loadGraphFromDOT gFile
    let qGraph = ParserInputGraph()
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

let justParse (path:string) =
    printfn "Parse file %A" path
    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }
    let allTokens = seq []
    let res =
        let g = loadDotToQG path
        (new Yard.Generators.RNGLR.AbstractParser.Parser<_>()).Parse buildAstAbstract g
    res

let p = new ProjInfo()
let mutable counter = 1<id>
let resultDirectoryPath = ref @"../../result\"

let getResultFileName (path : string) pref  = 
    !resultDirectoryPath + pref + path.Substring(path.LastIndexOf("\\") + 1)

let Parse (srcFilePath:string) = 
    let StreamElement = new StreamReader(srcFilePath, System.Text.Encoding.UTF8)  
    let map = p.GetMap StreamElement
    p.AddLine counter map
    counter <- counter + 1<id>
    match justParse srcFilePath with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg, dbg) ->
        let data =
            let d = tokenData tok
            if isLiteral tok then ""
            else (d :?> SourceText).text
        let name = tok |> tokenToNumber |> numToString
        printfn "Error in file %s on position  on Token %s %s: %s" srcFilePath name data msg
        dbg.drawGSSDot (getResultFileName srcFilePath "stack_")
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
        defaultAstToDot ast (getResultFileName srcFilePath "ast_")
       
let CreateEmptyResultDirectory = 
    if (System.IO.Directory.Exists(!resultDirectoryPath))
    then System.IO.Directory.Delete(!resultDirectoryPath, true)
    System.IO.Directory.CreateDirectory(!resultDirectoryPath)
        
let ParseAllDirectory (directoryName:string) =
    resultDirectoryPath := directoryName + @"\results\"
    CreateEmptyResultDirectory |> ignore
    System.IO.Directory.GetFiles(directoryName,"*.dot")
    |> Array.iter Parse

do 
    let inPath = ref @"..\..\tests\s2.dot"
    let parseDir = ref false
    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> inPath := path s), "Input file."
         "-d", ArgType.String (fun s -> parseDir := true; inPath := s), "Input dir. Use for parse all files in specified directory."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs

    !inPath
    |> if !parseDir
       then ParseAllDirectory
       else Parse