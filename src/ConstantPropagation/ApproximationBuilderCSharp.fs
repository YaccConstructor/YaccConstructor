module ApproximationBuilderCSharp

open XMLParser
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open System.Collections.Generic
open JetBrains.ReSharper.Psi.ControlFlow

let private hotspotInfoList = parseXml "Hotspots.xml"

let private tryDefineLang (node: IInvocationExpression) = 
    let typeDecl = node.InvokedExpression.GetText().Split('.')
    let className = typeDecl.[0].ToLowerInvariant()
    let methodName = typeDecl.[1].ToLowerInvariant()
                
    let args = node.AllArguments false
    let argTypes = new ResizeArray<_>()
    for argument in args do
        argTypes.Add <| argument.GetExpressionType().GetLongPresentableName(CSharpLanguage.Instance).ToLowerInvariant()
        
    let retType = node.GetExpressionType().GetLongPresentableName(CSharpLanguage.Instance).ToLowerInvariant()

    hotspotInfoList
    |> List.tryFind 
        (
            fun hotspotInfo -> 
                let hotspot = snd hotspotInfo
                hotspot.Class = className 
                && hotspot.Method = methodName 
                && argTypes.[hotspot.QueryPosition] = "string" 
                && hotspot.ReturnType = retType
        )
    |> Option.map fst  

// Searches control flow graph for IControlFlowElement element 
// correponding to passed IInvocationExpression node
let private findNodeInCFG (cfg: ICSharpControlFlowGraf) (node: IInvocationExpression) =
    if cfg = null then None
    else
        let rec dfs (elem: IControlFlowElement) =
            match elem with
            | null -> None
            | e when LanguagePrimitives.PhysicalEquality (node :> ITreeNode) e.SourceElement -> 
                Some (e)
            | e when e.Exits <> null ->
                e.Exits
                |> List.ofSeq
                |> List.map (fun rib -> rib.Target) 
                |> List.tryPick dfs
            | _ -> None
        dfs cfg.EntryElement
    
let private getEnclosingMethodNullParentMsg = "can't get enclosing method, null parent encountered"
let private hotspotElementNotFoundMsg = "hotspot's corresponding IControlFlowElementnot found"

let private getDataFlowGraph (hotspot: IInvocationExpression) =
    let rec getEnclosingMethod (node: ITreeNode) =
        match node with
        | null -> failwith getEnclosingMethodNullParentMsg
        | :? ICSharpFunctionDeclaration as funDecl -> funDecl
        | _ -> getEnclosingMethod node.Parent
    let methodDeclaration = getEnclosingMethod hotspot
    let controlFlowGraph = CSharpControlFlowBuilder.Build methodDeclaration
    let cfgHotElementOpt = findNodeInCFG controlFlowGraph hotspot
    match cfgHotElementOpt with
    | None -> failwith hotspotElementNotFoundMsg
    | Some cfgHotElem -> ()

let build (file: ICSharpFile) = 
    let hotspots = new ResizeArray<_>() 
    let addHotspot (node: ITreeNode) =
        match node with 
        | :? IInvocationExpression as invocExpr  -> 
            tryDefineLang invocExpr
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()
    let processor = RecursiveElementProcessor(fun x -> addHotspot x)
    processor.Process file
    hotspots |> List.ofSeq |> List.iter (getDataFlowGraph << snd)