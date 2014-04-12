module AbstractAnalysis.Common

exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

//----------------------------------
[<InjectedLanguage("TSQL")>]
type MyClass() = 
    member this.field = "lalala"