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

module YC.ReSharper.AbstractAnalysis.Languages.TSQL

open Yard.Examples.MSParser
open LexerHelper
open System
open System.IO
open Yard.Generators.Common.AST
open YC.SDK.CommonInterfaces
open YC.SDK.ReSharper.Helper
open Mono.Addins
open ReSharperExtension
open JetBrains.Application
open YC.FST.AbstractLexing.Interpreter

let tokenize lexerInputGraph =
    let eof = RNGLR_EOF(new GraphTokenValue<_>())    
    YC.TSQLLexer.tokenize eof lexerInputGraph

let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

let getTokenName = tokenToNumber >> numToString

let parse = fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph

let args = 
    {
        tokenToRange = fun x -> new GraphTokenValue<_>(),new GraphTokenValue<_>()
        zeroPosition = new GraphTokenValue<_>()
        clearAST = false
        filterEpsilons = true
    }

let printAstToDot ast name = defaultAstToDot ast name
let printOtherAstToDot sppf name = otherAstToDot sppf name

let langName = "TSQL"
let xmlPath = xmlPath
let tokenToTreeNode = tokenToTreeNode
let translate ast errors = translate args ast errors



[<assembly:Addin>]
[<assembly:AddinDependency ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]
do()

[<ShellComponent>]
[<Extension>]
type TSQLInjectedLanguageModule () =
    let processor = new Processor<Token, br, range, node>(tokenize, parse, translate, tokenToNumber, numToString, tokenData, tokenToTreeNode, langName, calculatePos, getRange, printAstToDot, printOtherAstToDot, None)

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