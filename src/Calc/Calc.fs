namespace YC.ReSharper.AbstractAnalysis.Languages.Calc

open Calc.AbstractParser
open AbstractLexer.Core
open Yard.Generators.RNGLR.AST
open YC.AbstractAnalysis.CommonInterfaces
open YC.ReSharper.AbstractAnalysis.Plugin.Core
open Mono.Addins
open YC.EL.ReSharper.Common
open ReSharperExtension
open JetBrains.Application

[<assembly:Addin>]
[<assembly:AddinDependency ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]

do()

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression

[<Extension>]
[<ShellComponent>]
type CalcInjectedLanguageModule () =
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
        let eof = Calc.AbstractParser.RNGLR_EOF("",[||])
        Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eof)

    let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

    let parse (*parser:Yard.Generators.RNGLR.AbstractParser.Parser<_>*) =
    
        fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph

    let args = 
        {
            tokenToRange = fun _ -> 0UL,0UL
            zeroPosition = 0UL
            clearAST = false
            filterEpsilons = true
        }

    let printAstToDot ast name = defaultAstToDot ast name
    let tokenToTreeNode = tokenToTreeNode
    let xmlPath = xmlPath
    let translate ast errors = translate args ast errors

    let processor =
        new Processor<Token,br,range,node>(tokenize, parse, translate, tokenToNumber, numToString, tokenData, tokenToTreeNode,"calc",calculatePos
                      , getRange)
    
    interface IInjectedLanguageModule<br,range,node> with
        member this.Name = "calc"
        member this.Process graphs = processor.Process graphs
        member this.LexingFinished = processor.LexingFinished
        member this.ParsingFinished = processor.ParsingFinished
        member this.XmlPath = xmlPath
        member this.GetNextTree i = processor.GetNextTree i
        member this.GetForestWithToken range = processor.GetForestWithToken range
        member this.GetPairedRanges left right range toRight = processor.GetPairedRanges left right range toRight

    interface IReSharperLanguage