namespace YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open System
open System.Diagnostics

module CFA =
    let private printCFGInfo (cfg: ICSharpControlFlowGraf) =
        Debug.WriteLine ("All elements count: " + cfg.AllElementsCount.ToString())
        Debug.WriteLine ("All edges count: " + cfg.EdgesCount.ToString())

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