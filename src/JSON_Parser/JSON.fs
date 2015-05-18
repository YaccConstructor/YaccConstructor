module YC.ReSharper.AbstractAnalysis.Languages.JSON

open System.IO
open AbstractAnalysis.Common
open JSON.Parser
open Yard.Generators.Common.AST
open YC.SDK.ReSharper.Helper
open YC.SDK.CommonInterfaces
open JetBrains.Application
open ReSharperExtension
open YC.FST.AbstractLexing.Interpreter

let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

let tokenize lexerInputGraph =
    let eof = RNGLR_EOF(new GraphTokenValue<_>())    
    YC.JSONLexer.tokenize eof lexerInputGraph

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
let printOtherAstToDot ast name = otherAstToDot ast name

let langName = "json"
let xmlPath = xmlPath
let tokenToTreeNode = tokenToTreeNode
let translate ast errors = translate args ast errors

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression

[<ShellComponent>]
type JSONInjectedLanguageModule () =
    let processor = new Processor<Token, br, range, node>(tokenize, parse, translate, tokenToNumber, numToString, tokenData, tokenToTreeNode, langName, calculatePos, getRange, printAstToDot, printOtherAstToDot)
    interface IInjectedLanguageModule<br, range, node> with
        member this.Name = langName
        member this.Process graphs = processor.Process graphs
        member this.LexingFinished = processor.LexingFinished
        member this.ParsingFinished = processor.ParsingFinished
        member this.XmlPath = xmlPath
        member this.GetNextTree i = processor.GetNextTree i
        member this.GetForestWithToken range = processor.GetForestWithToken range
        member this.GetPairedRanges left right range toRight = processor.GetPairedRanges left right range toRight

    interface IReSharperLanguage