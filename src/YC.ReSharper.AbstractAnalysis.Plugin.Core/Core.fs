module YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages
open Yard.Examples.MSParser
open AbstractAnalysis.Common

open System
open Mono.Addins

AddinManager.Initialize()
AddinManager.Registry.Update(null)

let injectedLanguages = 
    AddinManager.GetExtensionObjects (typeof<IInjectedLanguageProcessor>)
    |> Seq.cast<IInjectedLanguageProcessor>
    |> Seq.map (fun x -> x.Name,x)
    |> dict

//let AddinJSONNames = Seq.map (fun (elem : IInjectedLanguageProcessor) -> elem.Name) AddinJSON

type Processor(file) =
    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> injectedLanguages.["TSql"]
            | "eval" -> injectedLanguages.["Calc"]
            | "objnotation" -> injectedLanguages.["JSON"] 
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"

(*    let processLang graph tokenize parse addLError addPError = 
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
             | Yard.Generators.RNGLR.Parser.Success(_,_) -> ()
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )
    *)
            
//(provider: ICSharpContextActionDataProvider) = 
(*    member this.Process () = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
            brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
            res.ToArray()
        //let sourceFile = provider.SourceFile
        //let file = provider.SourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) :?> ICSharpFile
        let graphs = (new Approximator(file)).Approximate defLang
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
*)
        
        //let error (l:IInjectedLanguageProcessor) tok  = addError tok l.TokenToNumber l.NumToString l.TokenData 
        //let errorJSON tok  = addError tok JSON.Parser.tokenToNumber JSON.Parser.numToString JSON.Parser.tokenData

        (*graphs
        |> ResizeArray.iter 
            (fun (l,g) -> processLang g l.Tokenize l.Parse lexerErrors.Add  (error l))
                //match l with
                (*| Calc -> processLang g l.tokenize l.parse lexerErrors.Add  error Calc*)

        lexerErrors,parserErrors*)