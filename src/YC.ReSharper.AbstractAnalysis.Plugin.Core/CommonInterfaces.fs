module YC.AbstractAnalysis.CommonInterfaces

open Mono.Addins
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.Files
open JetBrains.Application.Progress
open JetBrains.ProjectModel

//[<assembly:Addin>]
 //ToDo

type LexingFinishedArgs (tokens : ResizeArray<ITreeNode>, lang:string) =
     inherit System.EventArgs()

     member this.Tokens = tokens
     member this.Lang = lang

type ParsingFinishedArgs() = 
    inherit System.EventArgs()
    member this.Lang = ""


exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language


[<Interface>]
type IInjectedLanguageProcessor<'token,'expression> =
    abstract Name: string
    abstract NumToString : int -> string
    abstract TokenToNumber: 'token -> int
    abstract TokenData: 'token -> obj
    abstract Tokenize : AbstractLexer.Common.LexerInputGraph<'expression> -> AbstractParsing.Common.ParserInputGraph<'token>
    abstract Parse : AbstractParsing.Common.ParserInputGraph<'token> -> Yard.Generators.RNGLR.Parser.ParseResult<'token>


[<Interface>]
[<TypeExtensionPoint>]
type IInjectedLanguageModule =    
     abstract Name: string
     abstract LexingFinished: IEvent<LexingFinishedArgs>
     abstract ParsingFinished: IEvent<ParsingFinishedArgs>
     abstract XmlPath: string
     abstract GetNextTree: int -> ITreeNode*bool
     abstract GetForestWithToken: JetBrains.DocumentModel.DocumentRange -> ResizeArray<ITreeNode>
     abstract Process
        : AbstractLexer.Common.LexerInputGraph<ICSharpLiteralExpression>
          -> ResizeArray<string * JetBrains.DocumentModel.DocumentRange> * ResizeArray<string * JetBrains.DocumentModel.DocumentRange>


