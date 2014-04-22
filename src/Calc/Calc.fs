module YC.ReSharper.AbstractAnalysis.Languages.Calc

open Calc.AbstractParser
open AbstractLexer.Common
open AbstractLexer.Core
open Yard.Generators.RNGLR.AST

let printTag tag printBrs = 
    match tag with
        | NUMBER(v,br) -> "NUM: " + v + "; br= " + printBrs br
        | PLUS(v,br)   
        | MULT(v,br)   
        | RBRACE(v,br)
        | POW(v,br)
        | DIV(v,br)
        | LBRACE(v,br) ->  v + "; br= " + printBrs br
        | e -> string e

let tokenize lexerInputGraph =
    let eof = Calc.AbstractParser.RNGLR_EOF("",[||])
    Calc.Lexer._fslex_tables.Tokenize(Calc.Lexer.fslex_actions_token, lexerInputGraph, eof)

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
let xmlPath = xmlPath
let translate ast errors = translate args ast errors