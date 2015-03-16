namespace YC.ReSharper.AbstractAnalysis.Languages.Calc

open ExtCalc.AbstractParser
open Yard.Generators.Common.AST
open YC.SDK.CommonInterfaces
open Mono.Addins
open YC.SDK.ReSharper.Helper
open ReSharperExtension
open JetBrains.Application
open YC.FST.AbstractLexing.Interpreter
open ControlFlowGraph

[<assembly:Addin>]
[<assembly:AddinDependency ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]

do()

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression

[<Extension>]
[<ShellComponent>]
type ExtCalcInjectedLanguageModule () =
    let tokenize lexerInputGraph =
        let eof = RNGLR_EOF(new GraphTokenValue<_>())    
        YC.ExtCalcLexer.tokenize eof lexerInputGraph

    let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

    let parse =
    
        fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph

    let args = 
        {
            tokenToRange = fun _ -> 0UL,0UL
            zeroPosition = 0UL
            clearAST = false
            filterEpsilons = true
        }
    
    let printAstToDot ast name = defaultAstToDot ast name
    
    let langName = "extcalc"
    let xmlPath = xmlPath
    let tokenToTreeNode = tokenToTreeNode
    let translate ast errors = translate args ast errors

    // variables for some semantic actions
    let semicolonNumber = tokenToNumber <| Token.SEMI (new GraphTokenValue<br>())
    let eqNumber = tokenToNumber <| Token.EQ (new GraphTokenValue<br>())
    let isVariable = 
        fun n -> 
            let num2 = tokenToNumber <| Token.VARIABLE (new GraphTokenValue<br>())
            n = num2

    let nodeToType = dict["assign", Assignment;]
    let typeToDelimiters = dict [Assignment, [semicolonNumber]; ]
    let langSource = new LanguageSource(nodeToType, typeToDelimiters, -1, -1, eqNumber, isVariable)

    let parserSource = new ParserSource<Token>(tokenToNumber, numToString, leftSide, tokenData)
    let semantic = Some <| (parserSource, langSource)

    let processor =
        new Processor<Token, br, range, node>(tokenize, parse, translate, tokenToNumber, numToString, tokenData, tokenToTreeNode, langName, calculatePos
                      , getRange, printAstToDot, otherAstToDot, semantic)
    
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