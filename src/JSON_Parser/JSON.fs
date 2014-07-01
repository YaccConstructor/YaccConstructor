module YC.ReSharper.AbstractAnalysis.Languages.JSON

open System.IO

open AbstractAnalysis.Common
open AbstractLexer.Core
open JSON.Parser
open Yard.Generators.RNGLR.AST
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
let translate ast errors = translate args ast errors

type JSONPars = 
    interface IInjectedLanguageProcessor<JSON.Parser.Token,JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression> with
        member this.Name = "JSON"
        member this.Parse (inG) = parse (inG)
        member this.NumToString (int) = JSON.Parser.numToString(int)
        member this.TokenData(token) = JSON.Parser.tokenData(token)
        member this.TokenToNumber(token) = JSON.Parser.tokenToNumber(token)
        member this.Tokenize(inG) = tokenize inG
