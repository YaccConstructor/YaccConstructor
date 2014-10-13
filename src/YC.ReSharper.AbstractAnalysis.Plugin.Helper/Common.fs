// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module YC.EL.ReSharper.Common

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open YC.FST.AbstractLexing.Interpreter
 
type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression
type range = JetBrains.DocumentModel.DocumentRange
type node = JetBrains.ReSharper.Psi.Tree.ITreeNode 

let getRange =  fun (x:JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression) -> (x:>ITreeNode).GetDocumentRange()

let calculatePos (grToken: GraphTokenValue<#ITreeNode>) =    
        let ranges = 
            grToken.Edges |> Seq.groupBy (fun x -> x.BackRef)
            |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
            |> Seq.map(fun grToken ->
                try
                    let pos =  grToken |> Array.map(fun i -> i.StartPos)
                    let lengthTok = pos.Length
                    let beginPosTok = pos.[0]
                    let endPosTok = pos.[lengthTok-1] + 1 
                    let endPos = 
                        grToken.[0].BackRef.GetDocumentRange().TextRange.EndOffset - endPosTok 
                        - grToken.[0].BackRef.GetDocumentRange().TextRange.StartOffset 
                    grToken.[0].BackRef.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
                with
                | e -> 
                    grToken.[0].BackRef.GetDocumentRange())
        ranges