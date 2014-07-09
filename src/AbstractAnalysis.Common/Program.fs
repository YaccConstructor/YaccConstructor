module AbstractAnalysis.Common

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
open JetBrains.ReSharper.Feature.Services.Bulbs
//open YC.ReSharper.AbstractAnalysis.Plugin.Core

//[<assembly:Addin>]
[<assembly:AddinRoot ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]
do() //ToDo

exception LexerError of string*obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

//----------------------------------
//[<InjectedLanguage("TSQL")>]
//type MyClass() = 
//    member this.field = "lalala

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
     abstract Process: ICSharpFile -> ResizeArray<string * JetBrains.DocumentModel.DocumentRange> * ResizeArray<string * JetBrains.DocumentModel.DocumentRange>

let processLang graph tokenize parse addLError addPError = 
    let tokenize g =
        try 
            tokenize g
            |> Some 
        with
        | LexerError(t,brs) ->
            (t, (brs :?> array<AbstractLexer.Core.Position<ICSharpLiteralExpression>>).[0].back_ref.GetDocumentRange())
            |> addLError
            None
    tokenize graph |> Option.map parse
    |> Option.iter
        (function 
            | Yard.Generators.RNGLR.Parser.Success(_,_,_) -> ()
            | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
        )


let HandleErrors (ilp:IInjectedLanguageProcessor<_,_>) file = 
    let parserErrors = new ResizeArray<_>()
    let lexerErrors = new ResizeArray<_>()
    let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
        let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
        brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
        res.ToArray()

    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> "TSQL" //injectedLanguages.["TSql"]
            | "eval" -> "Calc" //injectedLanguages.["Calc"]
            | "objnotation" -> "JSON" //injectedLanguages.["JSON"] 
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"

    let graphs = (new Approximator(file)).Approximate defLang //ToDo defLang to Core
    let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
        let ranges = 
            brs |> Seq.groupBy (fun x -> x.back_ref)
            |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
            |> Seq.map(fun brs ->
                try
                    let pos =  brs |> Array.map(fun i -> i.pos_cnum)
                    let lengthTok = pos.Length
                    let beginPosTok = pos.[0] + 1
                    let endPosTok = pos.[lengthTok-1] + 2 
                    let endPos = 
                        brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
                        - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
                    brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
                with
                | e -> 
                    brs.[0].back_ref.GetDocumentRange())
        ranges

    let addError tok tokenToNumberFunc numToStringFunc (tokenDataFunc: _ -> obj)= 
        let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
            calculatePos brs 
            |> Seq.iter
                (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
        let name = tok |> (tokenToNumberFunc >>  numToStringFunc)
        let l,br = tokenDataFunc tok :?>_
        e name l br

    let error tok  = addError tok ilp.TokenToNumber ilp.NumToString ilp.TokenData 
    
    graphs
    |> ResizeArray.iter 
        (fun (l,g) -> processLang g ilp.Tokenize ilp.Parse lexerErrors.Add  (error l)) //(error token) //ToDo
            (*match l with
            | Calc -> processLang g l.tokenize l.parse lexerErrors.Add  error Calc*)

    lexerErrors,parserErrors