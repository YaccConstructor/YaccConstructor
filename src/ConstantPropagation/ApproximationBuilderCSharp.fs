module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilderCSharp

open QuickGraph

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow

open XMLParser
open DataDependencyGraph

open System.Collections.Generic
open Microsoft.FSharp.Collections
open System.IO
open Utils


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

let createAstCfgMap (cfg: ICSharpControlFlowGraf): Dictionary<ITreeNode, HashSet<IControlFlowElement>> =
    let rec dfs (cfgElem: IControlFlowElement) (visited: HashSet<int>) 
                (dict: Dictionary<ITreeNode, HashSet<IControlFlowElement>>) =
        if cfgElem <> null && (not << visited.Contains) cfgElem.Id
        then
            visited.Add cfgElem.Id |> ignore
            let node = cfgElem.SourceElement
            if node <> null
            then
                if dict.ContainsKey node
                then dict.[node].Add cfgElem |> ignore
                else 
                    let newSet = new HashSet<IControlFlowElement>([cfgElem])
                    dict.[node] <- newSet
            cfgElem.Exits
            |> List.ofSeq
            |> List.map (fun rib -> rib.Target)
            |> List.iter (fun e -> dfs e visited dict)

    let dict = new Dictionary<ITreeNode, HashSet<IControlFlowElement>>()
    let visited = new HashSet<int>()
    dfs cfg.EntryElement visited dict
    dict
            

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
                    let path = Path.Combine (myDebugFolderPath, cfgName + ".dot")
                    DDGraphFuncs.toDot ddGraph cfgName path
                    ddGraph
            )
    ddGraphs