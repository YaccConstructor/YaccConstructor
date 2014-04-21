module AbstractAnalysis.Common

exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

//----------------------------------
[<InjectedLanguage("TSQL")>]
type MyClass() = 
    member this.field = "lalala"
    

type IParser =
    abstract NumToString : int -> string
    abstract TokenToNumber<'a> : 'a -> int //Token -> int
    abstract TokenData<'a> : 'a -> obj //Token -> Obj
    abstract Tokenize<'a, 'b> : 'a -> 'b //a -> AbstractParsing.Common.ParserInputGraph<Token>
    abstract Parse<'a, 'b> : 'a -> 'b