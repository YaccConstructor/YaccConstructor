module YC.FST.AbstractLexing.Tests.CommonTestChecker

//open AbstractParser.Tokens
open YC.FST.AbstractLexing.Interpreter
open YC.FSA.FsaApproximation
open YC.FSA.GraphBasedFsa
open NUnit.Framework
open Microsoft.FSharp.Collections 
open QuickGraph 
open AbstractAnalysis.Common
open Graphviz4Net.Dot.AntlrParser
open Graphviz4Net.Dot
open System.IO

let eof = AbstractParser.Tokens.RNGLR_EOF(new FSA<_>())   

let printTok =
     fun x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])

let checkGraph (graph:AdjacencyGraph<_,_>) countE countV  =
    Assert.AreEqual(graph.EdgeCount, countE, "Count of edges not equal expected number. ")
    Assert.AreEqual(graph.VertexCount, countV, "Count of vertices not equal expected number. ")

let printSmbString (x:char*Position<_>) = 
        (fst x).ToString() + "_br: " + (snd x).back_ref + "(" + (snd x).start_offset.ToString() + "," + (snd x).end_offset.ToString() + ")"

let printBref printSmbString =       
    let printGr (gr:FSA<_>) = 
        let strs = ref ""
        for edge in gr.Edges do
            strs := !strs + "[" + edge.Source.ToString() + ", " + 
                                  "{" + (match edge.Tag with |Smbl x -> printSmbString x |_ -> "") + "}" + edge.Target.ToString() + "] ;"
        !strs        
             
    fun x ->
        match x with
            | AbstractParser.Tokens.NUMBER(gr) -> "NUM: " + printGr gr
            | AbstractParser.Tokens.MINUS(gr) -> "MINUS: " + printGr gr
            | AbstractParser.Tokens.LBRACE(gr) -> "LBRACE: " + printGr gr
            | AbstractParser.Tokens.RBRACE(gr) -> "RBRACE: " + printGr gr
            | AbstractParser.Tokens.DIV(gr) -> "DIV: " + printGr gr
            | AbstractParser.Tokens.PLUS(gr) -> "PLUS: "  + printGr gr
            | AbstractParser.Tokens.POW(gr)  -> "POW: "  + printGr gr
            | AbstractParser.Tokens.MULT(gr) -> "MULT: " + printGr gr
            | AbstractParser.Tokens.LITERAL(gr) -> "LITERAL: " + printGr gr
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0])  

let printBrefTSQL printSmbString =       
    let printGr (gr:FSA<_>) = 
        let strs = ref ""
        for edge in gr.Edges do
            strs := !strs + "[" + edge.Source.ToString() + ", " + 
                                  "{" + (match edge.Tag with |Smbl x -> printSmbString x |_ -> "") + "}" + edge.Target.ToString() + "] ;"
        !strs        
             
    fun x ->
        match x with
            | TSQLParserToken.DEC_NUMBER(gr) -> "NUM: " + printGr gr
            | TSQLParserToken.L_comma_(gr) -> "COMMA: " + printGr gr
            | TSQLParserToken.L_equal_(gr) -> "EQUAL: " + printGr gr
            | TSQLParserToken.L_more_(gr) -> "MORE: " + printGr gr
            | TSQLParserToken.L_less_(gr) -> "LESS: " + printGr gr
            | TSQLParserToken.L_colon_(gr) -> "COLON: " + printGr gr
            | TSQLParserToken.L_left_bracket_(gr) -> "LEFT_BR: " + printGr gr
            | TSQLParserToken.L_right_bracket_(gr) -> "RIGHT_BR: " + printGr gr
            | TSQLParserToken.L_plus_(gr) -> "PLUS: " + printGr gr
            | TSQLParserToken.L_minus_(gr) -> "MINUS: " + printGr gr
            | TSQLParserToken.L_star_(gr) -> "STAR: " + printGr gr
            | TSQLParserToken.L_select(gr) -> "SELECT: " + printGr gr
            | TSQLParserToken.L_from(gr) -> "FROM: " + printGr gr
            | TSQLParserToken.L_where(gr) -> "WHERE: " + printGr gr
            | TSQLParserToken.L_and_(gr) -> "AND: " + printGr gr
            | TSQLParserToken.L_or_(gr) -> "OR: " + printGr gr
            | TSQLParserToken.IDENT(gr) -> "IDENT: " + printGr gr
            | TSQLParserToken.RNGLR_EOF(gr) -> "EOF: " + printGr gr
            | x -> string x  |> (fun s -> s.Split '+' |> Array.rev |> fun a -> a.[0]) 

let path baseInputGraphsPath name = System.IO.Path.Combine(baseInputGraphsPath,name)

let loadGraphFromDOT filePath =
    let parser = AntlrParserAdapter<string>.GetParser()
    parser.Parse(new StreamReader(File.OpenRead filePath))

let loadDotToQG baseInputGraphsPath gFile =
    let qGraph = loadGraphFromDOT(path baseInputGraphsPath gFile)
    let graphAppr = new Appr<_>()
    graphAppr.InitState <- ResizeArray.singleton 0

    for e in qGraph.Edges do
        let edg = e :?> DotEdge<string>
        new TaggedEdge<_,_>(int edg.Source.Id, int edg.Destination.Id, (edg.Label, edg.Label)) |> graphAppr.AddVerticesAndEdge |> ignore

    graphAppr.FinalState <- ResizeArray.singleton (Seq.max graphAppr.Vertices)
    graphAppr

let checkArr expectedArr actualArr =
    if Array.length expectedArr = Array.length actualArr
    then 
        Array.iteri2 (
            fun i x1 x2 -> 
            if x1 <> x2 then Assert.Fail ("Arrays differ at position: " + string i)) expectedArr actualArr
        Assert.Pass()
    else Assert.Fail ("Arrays have different length")
 
let countEdges (parserInputGraph : ParserInputGraph<_>) =
   parserInputGraph.Edges 
    |> Seq.map (fun e -> 
                    match e.Tag with
                        | AbstractParser.Tokens.NUMBER(gr) 
                        | AbstractParser.Tokens.MINUS(gr) 
                        | AbstractParser.Tokens.LBRACE(gr) 
                        | AbstractParser.Tokens.RBRACE(gr) 
                        | AbstractParser.Tokens.DIV(gr) 
                        | AbstractParser.Tokens.PLUS(gr) 
                        | AbstractParser.Tokens.POW(gr)  
                        | AbstractParser.Tokens.MULT(gr) 
                        | AbstractParser.Tokens.LITERAL(gr) -> gr.EdgeCount
                        | AbstractParser.Tokens.RNGLR_EOF _ -> 0) 
    |> Array.ofSeq 

let ToDot (parserInputGraph : ParserInputGraph<_>) filePrintPath toStr =
    let rank s l =
        "{ rank=" + s + "; " + (l |> string) + " }\n"
    let s = 
        "digraph G {\n" 
        + "rankdir = LR\n"
        + "node [shape = circle]\n"
        + sprintf "%i[style=filled, fillcolor=green]\n" parserInputGraph.InitState 
        + sprintf "%i[shape = doublecircle, style=filled, fillcolor=red]\n" parserInputGraph.FinalState
        + rank "same" parserInputGraph.InitState
        + rank "min" parserInputGraph.InitState  
        + rank "same" parserInputGraph.FinalState 
        + rank "max" parserInputGraph.FinalState
    
    let strs =
            parserInputGraph.Edges
            |> Seq.map (fun edge ->
                sprintf "%i -> %i [label=\"%s\"]; \n" edge.Source edge.Target  (toStr edge.Tag)) 
                                      
    System.IO.File.WriteAllText(filePrintPath, s + (String.concat "" strs) + "\n}")
    ()

