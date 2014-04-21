module YC.ReSharper.AbstractAnalysis.Languages.JSON

open System.IO

open AbstractAnalysis.Common
open AbstractLexer.Core
open JSON.Parser

let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()

let tokenize lexerInputGraph =
    let eof = RNGLR_EOF("",[||])
    Lexer._fslex_tables.Tokenize(Lexer.fslex_actions_token, lexerInputGraph, eof)


let parse (*parser:Yard.Generators.RNGLR.AbstractParser.Parser<_>*) =
    
    fun parserInputGraph -> parser.Parse buildAstAbstract parserInputGraph


type JSONPars = 
    interface IParser with
        //member this.Parse() = parse()
        member this.NumToString (int) = JSON.Parser.numToString(int)
        //member this.TokenData<'Token>() = JSON.Parser.tokenData(Token)
        //member this.TokenToNumber() = JSON.Parser.tokenToNumber()
        //member this.Tokenize() = tokenize