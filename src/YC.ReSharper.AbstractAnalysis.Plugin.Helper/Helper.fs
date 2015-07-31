module YC.SDK.ReSharper.Helper

open JetBrains.DocumentModel
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

open Microsoft.FSharp.Collections

open ReSharperExtension.YcIntegration
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation

type br = ICSharpLiteralExpression
type range = DocumentRange
type node = ITreeNode 

/// todo: this function code is very similar of code calculatePos function. It's bad. Need refactoring.
let getRange (pos : Position<br>) = 
    let startOffset = pos.start_offset
    let beginPosTok, endPosTok = startOffset + 1, startOffset + 2
    let grTokenBackRef = pos.back_ref
    let endPos = 
        grTokenBackRef.GetDocumentRange().TextRange.EndOffset - endPosTok 
        - grTokenBackRef.GetDocumentRange().TextRange.StartOffset 
    grTokenBackRef.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)

let addSemantic (parent : ITreeNode) (children : ITreeNode list) = 
    let mutable prev = null
    let mutable curr = null
    let ranges = new ResizeArray<DocumentRange>()

    for child in children do
        prev <- curr
        curr <- child
        curr.PersistentUserData.PutData(Constants.Parent, parent)
        ranges.AddRange (curr.UserData.GetData(Constants.Ranges))
        if prev = null
        then parent.PersistentUserData.PutData(Constants.FirstChild, curr)
        else
            prev.PersistentUserData.PutData(Constants.NextSibling, curr)
            curr.PersistentUserData.PutData(Constants.PrevSibling, prev)
    parent.PersistentUserData.PutData(Constants.LastChild, curr)
    parent.UserData.PutData(Constants.Ranges, ranges)
    
    if ranges <> null && ranges.Count > 0
    then parent.UserData.PutData(Constants.Document, ranges.[0].Document)
    
    parent

let calculatePos (grToken: FSA<char*Position<#ITreeNode>>) =    
    let ranges = 
        grToken.Edges 
        |> Seq.groupBy (fun x -> match x.Tag with |Smbl y -> (snd y).back_ref |_ -> failwith "Unexpected Eps!!") //x.BackRef)
        |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
        |> Seq.map(fun grToken ->
            try
                let pos =  grToken |> Array.map(fun i -> match i.Tag with |Smbl y -> (snd y).start_offset |_ -> failwith "Unexpected Eps!!") //i.StartPos)
                let lengthTok = pos.Length
                let beginPosTok = pos.[0] + 1
                let endPosTok = pos.[lengthTok-1] + 2
                let grTokenBackRef = match grToken.[0].Tag with |Smbl y -> (snd y).back_ref |_ -> failwith "Unexpected Eps!!" 
                let endPos = 
                    grTokenBackRef.GetDocumentRange().TextRange.EndOffset - endPosTok 
                    - grTokenBackRef.GetDocumentRange().TextRange.StartOffset 
                grTokenBackRef.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
            with
            | e -> 
                let grTokenBackRef = match grToken.[0].Tag with |Smbl y -> (snd y).back_ref |_ -> failwith "Unexpected Eps!!"
                grTokenBackRef.GetDocumentRange())
    ranges