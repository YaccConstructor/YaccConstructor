namespace YC.ReSharper.AbstractAnalysis.Languages.Calc

open Calc.AbstractParser
open Yard.Generators.Common.AST
open YC.SDK.CommonInterfaces
open Mono.Addins
open YC.SDK.ReSharper.Helper
open ReSharperExtension
open YC.FST.AbstractLexing.Interpreter
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open YC.FST.GraphBasedFst

open JetBrains.Application
open JetBrains.ReSharper.Psi.CSharp.Tree

[<assembly:Addin>]
[<assembly:AddinDependency ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]

do()

type br = ICSharpLiteralExpression

[<Extension>]
[<ShellComponent>]
type CalcInjectedLanguageModule () =
    let tokenize (lexerInputGraph:Appr<_>) =
        let graphFsa = lexerInputGraph.ApprToFSA()
        let eof = RNGLR_EOF(new FSA<_>())
        let transform x = (x, match x with |Smbl(y, _) -> Smbl y |_ -> Eps)
        let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
        let graphFst = FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)
        YC.CalcLexer.tokenize eof graphFst

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
    
    let langName = "calc"
    let xmlPath = xmlPath
    let tokenToTreeNode = tokenToTreeNode
    let translate ast errors = translate args ast errors

    let processor =
        new Processor<Token, br, range, node>(tokenize, parse, translate, tokenToNumber, numToString, tokenData, tokenToTreeNode, langName, calculatePos
                      , getRange, printAstToDot, otherAstToDot, None)
    
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