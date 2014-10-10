// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module YC.EL.ReSharper.Common

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
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