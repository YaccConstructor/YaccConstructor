module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilderCSharp

open QuickGraph

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow

open XMLParser
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ExtendedCFG.General
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ExtendedCFG.CSharp
open DataDependencyGraph

open System.Collections.Generic
open Microsoft.FSharp.Collections
open System.IO


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

let getHotspots (file: ICSharpFile) =
    let hotspots = new ResizeArray<_>() 
    let addHotspot (node: ITreeNode) =
        match node with 
        | :? IInvocationExpression as invocExpr  -> 
            tryDefineLang invocExpr
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()

    let processor = RecursiveElementProcessor(fun x -> addHotspot x)
    processor.Process file
    hotspots

let private getEnclosingMethodNullParentMsg = "can't get enclosing method, null parent encountered"

let private createControlFlowGraph (hotspot: IInvocationExpression) =
    let rec getEnclosingMethod (node: ITreeNode) =
        match node with
        | null -> failwith getEnclosingMethodNullParentMsg
        | :? ICSharpFunctionDeclaration as funDecl -> funDecl
        | _ -> getEnclosingMethod node.Parent

    let methodDeclaration = getEnclosingMethod hotspot
    CSharpControlFlowBuilder.Build methodDeclaration

//let build (file: ICSharpFile) = 
//    let hotspots = getHotspots file
//    let ddGraphOpts = 
//        hotspots 
//        |> List.ofSeq 
//        |> List.map (fun (lang, hotspot) -> createControlFlowGraph hotspot, hotspot)
//        |> List.map
//            (
//                fun (cfg, hotspot) ->
//                    // expects "node <> null"
//                    let isPsiElemEquals (node: ITreeNode) (cfgNode: IExtendedCFGNode) =
//                        LanguagePrimitives.PhysicalEquality node cfgNode.psiElem
//                    let extCFG = CSharpExtendedCFG(cfg) :> IExtendedCFG
//                    extCFG.findFirst (isPsiElemEquals hotspot)
//                    |> Option.map (fun node -> node.getAncestorsSubgraph (fun _ -> true))
//            )
//    ()

//open DDGraph
//
//let rec buildDDG (node: IControlFlowElement) (previousNode: int) (graph: DDGraph) =
//    // exception messages
//    let locVarDeclNotLinearMsg = @"non linear part is encountered while 
//                                   seeking for local variable declaration beginning"
//    let locVarDeclNullMsg = @"null node is encountered while 
//                              seeking for local variable declaration beginning"
//
//    let processAncestors (node: IControlFlowElement) =
//        node.Entries
//        |> List.ofSeq
//        |> List.choose(fun e -> if e.Source <> null then Some(e.Source) else None)
//        |> List.iter (fun src -> buildDDG src node.Id graph)
//
//    let rec skipLocVarDecl (node: IControlFlowElement) =
//        match node with
//        | null -> failwith locVarDeclNullMsg
//        | :? IDeclarationStatement  -> node
//        | _ -> 
//            if node.Entries.Count > 1 
//            then failwith locVarDeclNotLinearMsg
//            else skipLocVarDecl node.Entries.[0].Source
//
//    match node with 
//    | :? ILocalVariableDeclaration as locVarDecl ->
//        if locVarDecl.DeclaredElement.Type.IsString()
//        then
//            graph.AddNode node.Id node previousNode
//            processAncestors node
//        else
//            let nextNode = skipLocVarDecl node
//            processAncestors nextNode
//    | :? IDeclarationStatement -> 
//        // it is assumed that local var declaration is between 
//        // ILocalVariableDeclaration and IDeclarationStatement
//        processAncestors node
//    | :? ICSharpLiteralExpression as literal->
//        if literal.Type().IsString()
//        graph.AddNode node.Id node previousNode
//        processAncestors node

//let findFirst (cfg: ICSharpControlFlowGraf) (node: ITreeNode) =
//    if cfg = null then None
//    else
//        let rec dfs (elem: IControlFlowElement) =
//            match elem with
//            | null -> None
//            | e when LanguagePrimitives.PhysicalEquality node e.SourceElement -> 
//                Some (e)
//            | e when e.Exits <> null ->
//                e.Exits
//                |> List.ofSeq
//                |> List.map (fun rib -> rib.Target) 
//                |> List.tryPick dfs
//            | _ -> None
//        dfs cfg.EntryElement
//
//let findUsages (node: IControlFlowElement) (varName: string) =
//    let rec findUsagesImpl (node: IControlFlowElement) (varName: string) 
//                           (usages: ResizeArray<int * string * ITreeNode>) =
//        let processAncestors (node: IControlFlowElement) =
//            node.Entries
//            |> List.ofSeq
//            |> List.choose(fun e -> if e.Source <> null then Some(e.Source) else None)
//            |> List.map (fun src -> findUsagesImpl src varName usages)
//            |> List.head
//
//        if node = null
//        then usages
//        else 
//            let treeNode = node.SourceElement
//            match treeNode with
//            | :? ILocalVariableDeclaration as varDecl ->
//                if varDecl.NameIdentifier.Name = varName
//                then do usages.Add (node.Id, treeNode.NodeType.ToString(), treeNode)
//                usages
//            | :? IReferenceExpression as refExpr ->
//                if refExpr.NameIdentifier.Name = varName
//                then do usages.Add (node.Id, treeNode.NodeType.ToString(), treeNode)
//                processAncestors node
//            | _ -> processAncestors node
//
//    let usages = new ResizeArray<int * string * ITreeNode>()
//    findUsagesImpl node varName usages
//
//let outputResult (usages: ResizeArray<int * string * ITreeNode>) queryVarName =
//    use outStream = FileInfo(Path.Combine("E:\\Diploma\\Debug", queryVarName + "_usage.txt")).CreateText()
//    usages 
//    |> ResizeArray.iter (fun (id, usage, _) -> outStream.WriteLine(id.ToString() + " " + usage))
//
//type DDNode = {id: int; info: ITreeNode}
//    
//let buildSomethingPlease (node: IControlFlowElement) (varName: string) =
//    
//    let processInvocationExpr invocExpr graph = () // not implemented
//
//    let processLocVarDecl (locVarDecl: ILocalVariableDeclaration) (graph: ResizeArray<DDNode * DDNode>) = 
//        let initializer = locVarDecl.Initializer
//        let dst = {id = locVarDecl.GetHashCode(); info = locVarDecl}
//        let src = {id = initializer.GetHashCode(); info = initializer}
//        graph.Add (src, dst)
//        match initializer with
//        | :? ICSharpLiteralExpression as literalExpr -> () // no need to do anything
//        | :? IInvocationExpression as invocExpr ->
//            processInvocationExpr invocExpr graph
//        | :? IAdditiveExpression as addExpr ->
//            // recursively run all the algo (begenning from find)
//            
//
//    let rec buildSomethingPleaseImpl (node: IControlFlowElement) (varName: string) (graph: ResizeArray<DDNode * DDNode>) =
//        let processAncestors (node: IControlFlowElement) =
//            node.Entries
//            |> List.ofSeq
//            |> List.choose(fun e -> if e.Source <> null then Some(e.Source) else None)
//            |> List.map (fun src -> buildSomethingPleaseImpl src varName graph)
//            |> List.head
//
//        if node = null
//        then graph
//        else 
//            let treeNode = node.SourceElement
//            match treeNode with
//            | :? ILocalVariableDeclaration as varDecl ->
//                if varDecl.NameIdentifier.Name = varName
//                then do graph.Add node.Id
//                graph
//            | :? IReferenceExpression as refExpr ->
//                if refExpr.NameIdentifier.Name = varName
//                then do graph.Add node.Id
//                processAncestors node
//            | _ -> processAncestors node
//    ()
//
//
//let build2 (file: ICSharpFile) = 
//    let cfgHotspotIsNone = "cfg hotspot is none"
//
//    let hotspots = getHotspots file
//    hotspots 
//    |> List.ofSeq 
//    |> List.map (fun (lang, hotspot) -> createControlFlowGraph hotspot, hotspot)
//    |> List.iter
//        (
//            fun (cfg, hotspot) ->
//                let cfgHotspotOpt = findFirst cfg hotspot
//                if cfgHotspotOpt.IsNone 
//                then failwith cfgHotspotIsNone
//                else 
//                    let cfgHotspot = cfgHotspotOpt.Value
//                    let queryVarPosition = 0 // temporary
//                    let queryVar = cfgHotspot.Entries.[0].Source.Entries.[0].Source.SourceElement :?> IReferenceExpression
//                    let queryVarName = queryVar.NameIdentifier.Name
//                    let usages = findUsages cfgHotspot queryVarName
//                    outputResult usages queryVarName
//        )
//

let createAstCfgMap (cfg: ICSharpControlFlowGraf): Dictionary<ITreeNode, IControlFlowElement> =
    let rec dfs (elem: IControlFlowElement) (dict: Dictionary<ITreeNode, IControlFlowElement>) =
        if elem = null
        then dict
        else
            if elem.SourceElement <> null
            then dict.[elem.SourceElement] <- elem
            elem.Exits
            |> List.ofSeq
            |> List.map (fun rib -> rib.Target) 
            |> List.map (fun t -> dfs t dict)
            |> List.head
        
    let dict = new Dictionary<ITreeNode, IControlFlowElement>()
    dfs cfg.EntryElement dict

let build (file: ICSharpFile) =
    let hotspots = getHotspots file
    let ddGraphs = 
        hotspots 
        |> List.ofSeq 
        |> List.map (fun (lang, hotspot) -> createControlFlowGraph hotspot, hotspot)
        |> List.map
            (
                fun (cfg, hotspot) ->
                    let queryVarRef = hotspot.Arguments.[0].Value :?> IReferenceExpression
                    let astCfgMap = createAstCfgMap cfg
                    let ddGraph = DDGraphFuncs.buildForVar queryVarRef astCfgMap

                    let cfgName = "ddg_" + cfg.GetHashCode().ToString()
                    let path = Path.Combine ("E:\\Diploma\\Debug", cfgName + ".dot")
                    ddGraph.ToDot cfgName path
            )
    ()