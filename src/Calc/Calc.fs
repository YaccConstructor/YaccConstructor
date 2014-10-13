namespace YC.ReSharper.AbstractAnalysis.Languages.Calc

open Calc.AbstractParser
open Yard.Generators.RNGLR.AST
open YC.AbstractAnalysis.CommonInterfaces
open YC.ReSharper.AbstractAnalysis.Plugin.Core
open Mono.Addins
open YC.EL.ReSharper.Common
open ReSharperExtension
open JetBrains.Application
open YC.FST.AbstractLexing.Interpreter

[<assembly:Addin>]
[<assembly:AddinDependency ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]

do()

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression

[<Extension>]
[<ShellComponent>]
type CalcInjectedLanguageModule () =
    let tokenize lexerInputGraph =
        let eof = RNGLR_EOF(new GraphTokenValue<_>())    
        YC.CalcLexer.tokenize eof lexerInputGraph

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
                      , getRange, printAstToDot, otherAstToDot)
    
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