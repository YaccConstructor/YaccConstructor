module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation.CFA

// module for ReSharper Control Flow Analysis (CFA) capabilities exploration 

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open System
open System.Diagnostics
open System.Collections.Generic

let private printCFGInfo (cfg: ICSharpControlFlowGraf) =
//        let cfgElementsMap (elems: IEnumerator<IControlFlowElement>) f =
//            while elems.MoveNext() do f elems.Current
//        cfgElementsMap 
//            (cfg.AllElements.GetEnumerator())
//            (
//                fun (elem: IControlFlowElement) -> 
//                    if elem.Parent <> null && elem.Parent.SourceElement <> null 
//                    then Debug.WriteLine ("Par: " + elem.Parent.SourceElement.ToString())
//                    else Debug.WriteLine ("Par: null")
//                    if elem.SourceElement <> null 
//                    then Debug.WriteLine ("Cur: " + elem.SourceElement.ToString())
//                    else Debug.WriteLine ("Cur: null")
////                    cfgElementsMap
////                        (elem.Children.GetEnumerator())
////                        (fun ch -> Debug.WriteLine (" Ch: " + ch.SourceElement.ToString()))
//                    Debug.WriteLine ("")
//            )

    let printRib (rib: IControlFlowRib) =
        if rib <> null 
        then
            let src = 
                if rib.Source <> null then rib.Source.Id.ToString() else "null"
            let dst = 
                if rib.Target <> null then rib.Target.Id.ToString() else "null"
            Debug.Write (src + "->" + dst)
        else
            Debug.Write ("nullrib")

    let printGraphNode (node: IControlFlowElement) =
        Debug.WriteLine ("ID: " + node.Id.ToString())
        let srcElemStr = 
            if node.SourceElement <> null 
            then node.SourceElement.ToString() 
            else "null"
        Debug.WriteLine ("SrcElem: " + srcElemStr)
        let entries = List.ofSeq node.Entries
        Debug.Write ("In: ")
        entries |> List.iter (fun r -> printRib r; Debug.Write (" "))
        Debug.WriteLine ("")
        let exits = List.ofSeq node.Exits
        Debug.Write ("Out: ")
        exits |> List.iter (fun r -> printRib r; Debug.Write (" "))
        Debug.WriteLine ("\n")

    let rec printGraph (elems: list<IControlFlowElement>) =
        match elems with
        | null :: tl ->
            Debug.WriteLine ("null node\n")
            printGraph tl
        | hd :: tl -> 
            printGraphNode hd
            let updaterElems =
                tl @ (
                    hd.Exits 
                    |> List.ofSeq 
                    |> List.map (fun rib -> if rib <> null then rib.Target else null)
                )
            printGraph updaterElems
        | [] -> ()
    printGraph [cfg.EntryElement]
        

let analyze (file: ICSharpFile) =
    let analyzeMethod (methodDecl: IMethodDeclaration) =
        Debug.WriteLine ("CFG for method " + methodDecl.NameIdentifier.GetText())
        let cfg = CSharpControlFlowBuilder.Build methodDecl
        printCFGInfo cfg
    let processorAction (node: ITreeNode) = 
        match node with
        | :? IMethodDeclaration as methodDecl -> analyzeMethod methodDecl
        | _ -> ()
    let processor = RecursiveElementProcessor (fun node -> processorAction node)
    processor.Process file