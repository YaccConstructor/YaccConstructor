namespace YC.ReSharper.AbstractAnalysis.Languages.ExtCalc

open Mono.Addins

open System

open JetBrains.Application
open JetBrains.Application.BuildScript.Application.Zones
open JetBrains.ReSharper.Psi.CSharp.Tree

open ControlFlowGraph.Common
open ControlFlowGraph.InputStructures

open Yard.Generators.RNGLR.OtherSPPF
open ExtCalc.AbstractParser
open ReSharperExtension
open Yard.Generators.Common.AST
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation
open YC.FST.AbstractLexing.Interpreter
open QuickGraph.FST.GraphBasedFst
open YC.SDK.CommonInterfaces
open YC.SDK.ReSharper.Helper


[<ZoneMarker>]
type ZoneMarker() = class end

[<assembly:Addin>]
[<assembly:AddinDependency("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]

do()

[<Extension>]
[<ShellComponent>]
type ExtCalcInjectedLanguageModule() =
    let tokenize (lexerInputGraph : FSA<char * Position<'br>>) =
//        let graphFsa = lexerInputGraph.ApprToFSA()
        let eof = RNGLR_EOF(new FSA<_>())
        let transform x = 
            match x with 
            | Smbl(y : char, _) when y <> (char 65535) -> x, Smbl(int <| Convert.ToUInt32(y)) 
            | Smbl(y : char, _) when y = (char 65535)  -> x, Smbl 65535 
            | _ -> x, Eps
        let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
        let graphFst = FST<_,_>.FSAtoFST(lexerInputGraph, transform, smblEOF)
        YC.ExtCalcLexer.tokenize eof graphFst

    let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

    let parse = fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph

    let args = 
        {
            tokenToRange = fun _ -> 0UL,0UL
            zeroPosition = 0UL
            clearAST = false
            filterEpsilons = true
        }
    
    let printAstToDot ast name = defaultAstToDot ast name
    
    let otherAstToDot (otherAst : OtherTree<_>) name = 
        otherAst.ToDot numToString tokenToNumber leftSide name
    
    let langName = "extcalc"
    let tokenNames = Seq.ofList <| getLiteralNames @ getTerminalNames
    let tokenToTreeNode = tokenToTreeNode
    let translate ast errors = null//translate args ast errors

    // variables for some semantic actions
    let semicolonNumber = tokenToNumber <| Token.SEMI (new FSA<_>())
    let eqNumber = tokenToNumber <| Token.EQ (new FSA<_>())
    let isVariable = 
        fun n -> 
            let num2 = tokenToNumber <| Token.VARIABLE (new FSA<_>())
            n = num2

    let nodeToType = dict["assign", Assignment;]
    let keywordToInt = dict [
                                Keyword.SEMICOLON, semicolonNumber; 
                                Keyword.EQ, eqNumber;
                            ]
    let langSource = new LanguageSource(nodeToType, keywordToInt, isVariable)

    let tokToSourceString token = 
        let tok = (unbox <| tokenData token) :> FSA<char*Position<br>>
        
        calculatePos tok
        |> List.ofSeq
        |> List.map (fun range -> range.GetText())
        |> List.ofSeq
        |> List.fold (fun acc elem -> acc + elem) ""

    let parserSource = new CfgParserSource<Token>(tokenToNumber, numToString, leftSide, tokenData)
    let semantic = Some <| (parserSource, langSource, tokToSourceString)

    let processor =
        new Processor<Token, br, range, node>(tokenize, parse, translate, tokenToNumber
                        , numToString, tokenData, tokenToTreeNode, langName, calculatePos
                        , getRange, printAstToDot, otherAstToDot, semantic)
    
    interface IInjectedLanguageModule<br, range, node> with
        member this.Name = langName
        member this.Process graphs = processor.Process graphs
        member this.LexingFinished = processor.LexingFinished
        member this.ParsingFinished = processor.ParsingFinished
        member this.TokenNames = tokenNames
        //member this.GetNextTree i = processor.GetNextTree i
        //member this.GetForestWithToken range = processor.GetForestWithToken range
        member this.GetPairedRanges left right range toRight = processor.GetPairedRanges left right range toRight

    interface IReSharperLanguage