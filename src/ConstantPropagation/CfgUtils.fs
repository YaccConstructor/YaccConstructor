module CfgUtils

open QuickGraph

open JetBrains.ReSharper.Psi.ControlFlow

open System.Collections.Generic

open CSharpCFGInfo
open GenericCFG
open JetBrains.ReSharper.Psi.Tree

let collectAdditionalInfo (cfg: IControlFlowGraf) infoExtractor initInfo =
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
    dfs cfg.EntryElement infoExtractor visited initInfo

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

let convert (csharpCFG: IControlFlowGraf) toGenericNode =
    let rec dfs (cfe: IControlFlowElement) (visited: Set<int>) (genCFG: GenericCFG) =
        if Set.contains cfe.Id visited |> not
        then
            let visited' = Set.add cfe.Id visited
            let cur = toGenericNode cfe
            let children =
                cfe.Exits
                |> List.ofSeq
                |> List.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None) 
            children
            |> List.map (fun t -> toGenericNode t)
            |> List.iter(fun node -> genCFG.Graph.AddVerticesAndEdge(new Edge<CFGNode>(cur, node)) |> ignore)
            List.iter (fun ch -> dfs ch visited' genCFG) children

    let genericCFG = GenericCFGFuncs.create()
    dfs csharpCFG.EntryElement Set.empty genericCFG
    genericCFG

let correspondingCfe (treeNode: ITreeNode) (cfgInfo: CSharpCFGInfo) =
    let cfgNodes = cfgInfo.AstCfgMap.[treeNode.GetHashCode()]
    if cfgNodes.Count > 1
    then
        let multipleCfgNodesForAstNodeMsg = 
            "ast node maps to multiple cfg nodes where single mapping expected" 
        failwith multipleCfgNodesForAstNodeMsg
    else cfgNodes |> List.ofSeq |> List.head