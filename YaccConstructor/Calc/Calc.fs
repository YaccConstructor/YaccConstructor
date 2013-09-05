module YC.ReSharper.AbstractAnalysis.Languages.Calc

//open Graphviz4Net.Dot.AntlrParser
open System.IO
//open Graphviz4Net.Dot
//open QuickGraph
open AbstractLexer.Common
open AbstractLexer.Core
//open QuickGraph.Algorithms
open Calc.AbstractParser
//open QuickGraph.Algorithms
//open QuickGraph.Graphviz
//open Helpers



let printTag tag printBrs = 
    match tag with
        | NUMBER(v,br) -> "NUM: " + v + "; br= " + printBrs br
        | PLUS(v,br)   
        | MULT(v,br)   
        | RBRACE(v,br)
        | POW(v,br)
        | DIV(v,br)
        | LBRACE(v,br) ->  v + "; br= " + printBrs br
        | e -> string e

let tokenize lexerInputGraph =
    Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph)

let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

let parse (*parser:Yard.Generators.RNGLR.AbstractParser.Parser<_>*) =
    
    fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph
    