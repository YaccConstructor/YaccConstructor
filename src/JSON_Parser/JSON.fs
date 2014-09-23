module YC.ReSharper.AbstractAnalysis.Languages.JSON

open System.IO
open AbstractAnalysis.Common
open AbstractLexer.Core
open JSON.Parser
open Yard.Generators.RNGLR.AST
open YC.EL.ReSharper.Common
open Mono.Addins
open YC.AbstractAnalysis.CommonInterfaces
open YC.ReSharper.AbstractAnalysis.Plugin.Core
open JetBrains.Application
open ReSharperExtension

[<assembly:Addin>]
[<assembly:AddinDependency ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]
do()

let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

let tokenize lexerInputGraph =
    let eof = RNGLR_EOF("",[||])
    Lexer._fslex_tables.Tokenize(Lexer.fslex_actions_token, lexerInputGraph, eof)


let parse (*parser:Yard.Generators.RNGLR.AbstractParser.Parser<_>*) =
    
    fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph

let args = 
    {
        tokenToRange = fun _ -> [||],[||]
        zeroPosition = [||]
        clearAST = false
        filterEpsilons = true
    }

let printAstToDot ast name = defaultAstToDot ast name

let xmlPath = xmlPath
let tokenToTreeNode = tokenToTreeNode
let translate ast errors = translate args ast errors

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression

[<ShellComponent>]
[<Extension>]
type JSONInjectedLanguageModule () =
    let processor = new Processor<Token,br,range,node>(tokenize, parse, translate, tokenToNumber, numToString, tokenData, tokenToTreeNode, "calc", calculatePos, getRange)
    interface IInjectedLanguageModule<br,range,node> with
        member this.Name = "json"
        member this.Process graphs = processor.Process graphs
        member this.LexingFinished = processor.LexingFinished
        member this.ParsingFinished = processor.ParsingFinished
        member this.XmlPath = xmlPath
        member this.GetNextTree i = processor.GetNextTree i
        member this.GetForestWithToken range = processor.GetForestWithToken range
        member this.GetPairedRanges left right range toRight = processor.GetPairedRanges left right range toRight

    interface IReSharperLanguage