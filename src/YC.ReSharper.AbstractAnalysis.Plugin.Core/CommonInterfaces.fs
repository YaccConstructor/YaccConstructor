module YC.AbstractAnalysis.CommonInterfaces

open Mono.Addins
open Microsoft.FSharp.Collections

//[<assembly:Addin>]
 //ToDo

type LexingFinishedArgs<'node> (tokens : ResizeArray<'node>, lang:string) =
     inherit System.EventArgs()

     member this.Tokens = tokens
     member this.Lang = lang

type ParsingFinishedArgs(lang:string) = 
    inherit System.EventArgs()
    member this.Lang = lang


exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

[<Interface>]
[<TypeExtensionPoint>]
type IInjectedLanguageModule<'br,'range,'node> =    
     abstract Name: string
     abstract LexingFinished: IEvent<LexingFinishedArgs<'node>>
     abstract ParsingFinished: IEvent<ParsingFinishedArgs>
     abstract XmlPath: string
     abstract GetNextTree: int -> 'node*bool
     abstract GetForestWithToken: 'range -> ResizeArray<'node>
     abstract GetPairedRanges: int -> int -> 'range -> bool -> ResizeArray<'range>
     abstract Process
        : AbstractAnalysis.Common.LexerInputGraph<'br>
          -> ResizeArray<string * 'range> * ResizeArray<string * 'range>


