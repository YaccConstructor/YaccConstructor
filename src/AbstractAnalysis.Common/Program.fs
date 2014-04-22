module AbstractAnalysis.Common

exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

//----------------------------------
[<InjectedLanguage("TSQL")>]
type MyClass() = 
    member this.field = "lalala"
    

type IParser<'a,'b> =
    abstract NumToString : int -> string
    abstract TokenToNumber: 'a -> int //Token -> int
    abstract TokenData: 'a -> obj //Token -> Obj
    abstract Tokenize : AbstractLexer.Common.LexerInputGraph<'b> -> AbstractParsing.Common.ParserInputGraph<'a>
    abstract Parse : AbstractParsing.Common.ParserInputGraph<'a> -> Yard.Generators.RNGLR.Parser.ParseResult<'a>