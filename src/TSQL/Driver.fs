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

namespace YC.ReSharper.AbstractAnalysis.Languages.TSQL

open System
open System.IO

open Mono.Addins

open System

open JetBrains.Application
open JetBrains.Application.BuildScript.Application.Zones

open LexerHelper
open ReSharperExtension
open Yard.Generators.RNGLR.OtherSPPF
open Yard.Examples.MSParserAbstract
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

[<ShellComponent>]
[<Extension>]
type TSQLInjectedLanguageModule() =

    let tokenize (lexerInputGraph : FSA<char * Position<'br>>) =
//        let graphFsa = lexerInputGraph.ApprToFSA()
        let transform x = 
            match x with 
            | Smbl(y : char, _) when y <> (char 65535) -> x, Smbl(int <| Convert.ToUInt32(y)) 
            | Smbl(y : char, _) when y = (char 65535)  -> x, Smbl 65535 
            | _ -> x, Eps
        let smblEOF = Smbl(char 65535,  Unchecked.defaultof<Position<_>>)
        let graphFst = FST<_,_>.FSAtoFST(lexerInputGraph, transform, smblEOF)
        let eof = RNGLR_EOF(new FSA<_>())    
        YC.TSQLLexer.tokenize eof graphFst

    let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

    let getTokenName = tokenToNumber >> numToString

    let parse = fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph

    let args = 
        {
            tokenToRange = fun x -> new FSA<_>(),new FSA<_>()
            zeroPosition = new FSA<_>()
            clearAST = false
            filterEpsilons = true
        }

    let printAstToDot ast name = defaultAstToDot ast name
    
    let otherAstToDot (otherAst : OtherTree<_>) name = 
        otherAst.ToDot numToString tokenToNumber leftSide name

    let langName = "TSQL"
    let tokenNames = Seq.ofList <| getLiteralNames @ getTerminalNames
    let tokenToTreeNode = tokenToTreeNode
    let translate ast errors = null//translate args ast errors
    
    let processor = new Processor<Token, br, range, node>(tokenize, parse, translate, tokenToNumber
                        , numToString, tokenData, tokenToTreeNode, langName, calculatePos
                        , getRange, printAstToDot, otherAstToDot, None)

    interface IInjectedLanguageModule<br, range, node> with
        member this.Name = langName
        member this.Process graphs = processor.Process graphs
        member this.LexingFinished = processor.LexingFinished
        member this.ParsingFinished = processor.ParsingFinished
        member this.TokenNames = tokenNames
//        member this.GetNextTree i = processor.GetNextTree i
//        member this.GetForestWithToken range = processor.GetForestWithToken range
        member this.GetPairedRanges left right range toRight = processor.GetPairedRanges left right range toRight

    interface IReSharperLanguage
        