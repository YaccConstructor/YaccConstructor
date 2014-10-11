module YC.SDK.ReSharper.Helper

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.Application
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open ReSharperExtension
open Highlighting.Core

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression
type range = JetBrains.DocumentModel.DocumentRange
type node = JetBrains.ReSharper.Psi.Tree.ITreeNode 

let getRange =  fun (x:JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression) -> (x:>ITreeNode).GetDocumentRange()

let addSemantic (parent : ITreeNode) (children : ITreeNode list) = 
    let mutable prev = null
    let mutable curr = null
    let ranges = new ResizeArray<JetBrains.DocumentModel.DocumentRange>()
    for child in children do
        prev <- curr
        curr <- child
        curr.PersistentUserData.PutData(PropertyConstant.Parent, parent)
        ranges.AddRange (curr.UserData.GetData(KeyConstant.Ranges))
        if prev = null
        then parent.PersistentUserData.PutData(PropertyConstant.FirstChild, curr)
        else
            prev.PersistentUserData.PutData(PropertyConstant.NextSibling, curr)
            curr.PersistentUserData.PutData(PropertyConstant.PrevSibling, prev)
    parent.PersistentUserData.PutData(PropertyConstant.LastChild, curr)
    parent.UserData.PutData(KeyConstant.Ranges, ranges)
    if ranges <> null && ranges.Count > 0
    then parent.UserData.PutData(KeyConstant.Document, ranges.[0].Document)
    parent

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

[<Class>]
type ReSharperHelper<'range, 'node> private() =
    let getAllProcessors() =
        Shell.Instance.GetComponents<IReSharperLanguage>()

    let getProcessor (lang: string) = 
        let processors = getAllProcessors() |> Array.ofSeq
        let l = lang.ToLowerInvariant()
        processors
        |> Array.find (fun processor -> processor.Name.ToLowerInvariant() = l)
    
    static let instance = new ReSharperHelper<'range, 'node>()
    static member Instance = instance

    member this.XmlPath lang = (getProcessor lang).XmlPath
    member this.ParsingFinished = getAllProcessors() |> Seq.map (fun pr -> pr.ParsingFinished) |> (fun x -> new ResizeArray<_>(x))
    member this.GetNextTree lang i = (getProcessor lang).GetNextTree i
    member this.LexingFinished = getAllProcessors() |> Seq.map(fun pr -> pr.LexingFinished) |> (fun x -> new ResizeArray<_>(x))
    
    member this.GetForestWithToken lang range = (getProcessor lang).GetForestWithToken range
    member this.GetPairedRanges lang left right range toRight = (getProcessor lang).GetPairedRanges left right range toRight
    
    member this.Process(file) =
        let graphs = (new Approximator(file)).Approximate()
        let lexerErrors = new ResizeArray<_>()
        let parserErrors = new ResizeArray<_>()
        graphs
        |> ResizeArray.map (fun (lang, graph) -> (getProcessor lang).Process graph)
        |> ResizeArray.iter(fun (x, y) -> lexerErrors.AddRange x; parserErrors.AddRange y)
        
        lexerErrors, parserErrors