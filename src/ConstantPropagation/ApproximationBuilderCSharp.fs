module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilderCSharp

open QuickGraph

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow

open XMLParser
open CSharpCFGInfo
open DataDependencyGraph
open Utils
open CSharpCFGConversion
open GenericCFG
open GenericCFG.GenericCFGFuncs

open System.Collections.Generic
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

let extractCSharpCfgInfo (cfgElem: IControlFlowElement) (info: CSharpCFGInfo) =
    let wrongForNodeStructureMsg = 
        @"For loop structure assumption failed - 
        node with more than 2 in edges encountered"

    let addAstCfgMapping (cfgElem: IControlFlowElement) (map': Map<int, Set<ControlFlowElemWrapper>>) =
        let node = cfgElem.SourceElement
        if node <> null
        then
            let nodeHash = hash node
            let curSet =
                match Map.tryFind nodeHash map' with
                | Some(value) -> value
                | None -> Set.empty
            let updSet = Set.add (ControlFlowElemWrapper(cfgElem)) curSet
            Map.add nodeHash updSet map'
        else
            map'

    /// Searches first node with 2 in edges. Throws exception if 
    /// node with more than 2 in edges encountered (due to assumptions 
    /// about For loops cfg structure)
    let rec traverseDown (node: IControlFlowElement) (prev: IControlFlowElement) =
        if node.Entries.Count > 2
        then failwith wrongForNodeStructureMsg
        elif node.Entries.Count = 2
        then
            let prevNodeEntryIndex = 
                node.Entries
                    |> List.ofSeq
                    |> List.findIndex (fun en -> en.Source = prev)
            node, prevNodeEntryIndex
        else traverseDown node.Exits.[0].Target node

    let processForStmt (cfgElem: IControlFlowElement) (loopNodes: Map<int, LoopNodeInfo>) 
                       (astCfgMap: Map<int, Set<ControlFlowElemWrapper>>) =
        let nodeHash = hash cfgElem.SourceElement
        match Map.tryFind nodeHash astCfgMap with
        | Some(cfgSet) when cfgSet.Count = 3 ->
            let enterForNode =
                cfgSet
                |> Array.ofSeq
                |> Array.sort
                |> fun arr -> Array.get arr 0
                |> fun w -> w.Value
            let forNodeChild = enterForNode.Exits.[0].Target
            let loopNode, enterEdgeIndex = traverseDown forNodeChild enterForNode
            let bodyExitEdgeIndex = (enterEdgeIndex + 1) % 2
            let loopNodeInfo = { EnterEdgeIndex = enterEdgeIndex; BodyExitEdgeIndex = bodyExitEdgeIndex }
            Map.add loopNode.Id loopNodeInfo loopNodes
        | _ -> loopNodes
    
    let astCfgMap' = addAstCfgMapping cfgElem info.AstCfgMap
    let loopNodes' =
        if cfgElem.SourceElement :? IForStatement
        then processForStmt cfgElem info.LoopNodes astCfgMap'
        else info.LoopNodes
    { info with AstCfgMap = astCfgMap'; LoopNodes = loopNodes' }
            
let collectAdditionalInfo (cfg: ICSharpControlFlowGraf) =
    let rec dfs (cfgElem: IControlFlowElement) (extractInfo: IControlFlowElement -> 'Info -> 'Info) 
                (visited: HashSet<int>) (info: 'Info) =
        if cfgElem <> null && (not << visited.Contains) cfgElem.Id
        then
            visited.Add cfgElem.Id |> ignore
            let info' = extractInfo cfgElem info
            cfgElem.Exits
            |> List.ofSeq
            |> List.map (fun rib -> rib.Target)
            |> List.fold (fun accInfo e -> dfs e extractInfo visited accInfo) info'
        else
            info 

    let visited = new HashSet<int>()
    dfs cfg.EntryElement extractCSharpCfgInfo visited emptyCSharpCfgInfo

//let build (file: ICSharpFile) =
//    let hotspots = getHotspots file
//    let ddGraphs = 
//        hotspots 
//        |> List.ofSeq 
//        |> List.map (fun (lang, hotspot) -> createControlFlowGraph hotspot, hotspot)
//        |> List.map
//            (
//                fun (cfg, hotspot) ->
//                    let queryVarRef = hotspot.Arguments.[0].Value :?> IReferenceExpression
//                    let additionalInfo = collectAdditionalInfo cfg
//                    let ddGraph = DDGraphFuncs.buildForVar queryVarRef additionalInfo
//
//                    let cfgName = "ddg_" + cfg.GetHashCode().ToString()
//                    let path = Path.Combine (myDebugFolderPath, cfgName + ".dot")
//                    DDGraphFuncs.toDot ddGraph cfgName path
//                    ddGraph
//            )
//    ddGraphs

let build (file: ICSharpFile) =
    let hotspots = getHotspots file
    let ddGraphs = 
        hotspots 
        |> List.ofSeq 
        |> List.map (fun (lang, hotspot) -> createControlFlowGraph hotspot, hotspot)
        |> List.map
            (
                fun (csharpCFG, hotspot) ->
                    let additionalInfo = collectAdditionalInfo csharpCFG
                    let genericCFG = convert csharpCFG additionalInfo
                    let hotVarRef = extractVarRefFromHotspot hotspot additionalInfo genericCFG
                    let cfgForVar = subgraphForVar hotVarRef genericCFG

                    let cfgName = "cfg_" + csharpCFG.GetHashCode().ToString() + "_forvar"
                    let path = Path.Combine (myDebugFolderPath, cfgName + ".dot")
                    toDot cfgForVar cfgName path

                    cfgForVar
            )
    ddGraphs