namespace YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open System
open System.Diagnostics
open System.Collections.Generic

module CFA =
    let private printCFGInfo (cfg: ICSharpControlFlowGraf) =
//        let toList (enumerator: IEnumerator<'a>) = 
//            [ while enumerator.MoveNext() do yield enumerator.Current ]
        let cfgElementsMap (elems: IEnumerator<IControlFlowElement>) (f: IControlFlowElement -> Unit) =
            while elems.MoveNext() do f elems.Current
        cfgElementsMap 
            (cfg.AllElements.GetEnumerator())
            (
                fun (elem: IControlFlowElement) -> 
                    if elem.Parent <> null && elem.Parent.SourceElement <> null 
                    then Debug.WriteLine ("Par: " + elem.Parent.SourceElement.ToString())
                    else Debug.WriteLine ("Par: null")
                    if elem.SourceElement <> null 
                    then Debug.WriteLine ("Cur: " + elem.SourceElement.ToString())
                    else Debug.WriteLine ("Cur: null")
//                    cfgElementsMap
//                        (elem.Children.GetEnumerator())
//                        (fun ch -> Debug.WriteLine (" Ch: " + ch.SourceElement.ToString()))
                    Debug.WriteLine ("")
            )

    let buildControlFlowGraph (file: ICSharpFile) =
        let buildCFGImpl (methodDecl: IMethodDeclaration) =
            let cfg = CSharpControlFlowBuilder.Build methodDecl
            printCFGInfo cfg
        let processorAction (node: ITreeNode) = 
            match node with
            | :? IMethodDeclaration as methodDecl -> buildCFGImpl methodDecl
            | _ -> ()
        let processor = RecursiveElementProcessor (fun node -> processorAction node)
        processor.Process file