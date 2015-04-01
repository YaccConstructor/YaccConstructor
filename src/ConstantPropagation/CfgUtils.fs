module CfgUtils

open JetBrains.ReSharper.Psi.ControlFlow
open System.Collections.Generic
open CSharpCFGInfo

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