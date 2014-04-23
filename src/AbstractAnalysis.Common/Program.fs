module AbstractAnalysis.Common

exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

//----------------------------------
[<InjectedLanguage("TSQL")>]
type MyClass() = 
    member this.field = "lalala"
    

type IInjectedLanguageProcessor<'token,'expression> =
    abstract NumToString : int -> string
    abstract TokenToNumber: 'token -> int
    abstract TokenData: 'token -> obj
    abstract Tokenize : AbstractLexer.Common.LexerInputGraph<'expression> -> AbstractParsing.Common.ParserInputGraph<'token>
    abstract Parse : AbstractParsing.Common.ParserInputGraph<'token> -> Yard.Generators.RNGLR.Parser.ParseResult<'token>