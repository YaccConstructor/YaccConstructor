module IControlFlowGraphUtils

open JetBrains.ReSharper.Psi.ControlFlow

open GraphUtils

let cfgIsVisited (e: IControlFlowElement) v = Set.contains e.Id v
let cfgMakeVisited (e: IControlFlowElement) v = Set.add e.Id v

let basicCfgDfsParts preProcess processNode getNextNodes postProcess = {
    IsVisited = cfgIsVisited
    MakeVisited = cfgMakeVisited
    PreProcess = preProcess
    ProcessNode = processNode
    GetNextNodes = getNextNodes
    PostProcess = postProcess }

let notProcessNode e s = s

let cfgDfsParts processNode getNextNodes = 
    basicCfgDfsParts 
        notProcessNode
        processNode
        getNextNodes
        notProcessNode
        
let getCfeExits (e: IControlFlowElement) s =
    let exits =
        e.Exits
        |> Seq.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
        |> List.ofSeq
    exits, s

let cfgExitsDfsParts processNode =
    cfgDfsParts processNode getCfeExits

let getCfeEntries (e: IControlFlowElement) s =
    let entries =
        e.Entries
        |> Seq.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None)
        |> List.ofSeq
    entries, s

let cfgEntriesDfsParts processNode =
    cfgDfsParts processNode getCfeEntries
 
let findLoopNodes (cfg: IControlFlowGraf): Set<int> =
    let calculateEnterExit (cfg: IControlFlowGraf) =
        let initState = (0, Map.empty, Map.empty)
        let preProcess (e: IControlFlowElement) (step, enter, x) = 
            (step + 1, Map.add e.Id (step + 1) enter, x)
        let processNode e s = s
        let getNextNodes (e: IControlFlowElement) s =
            let exits =
                e.Exits
                |> Seq.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
                |> List.ofSeq
            exits, s
        let postProcess (e: IControlFlowElement) (step, x, exit) =
            (step + 1, x, Map.add e.Id (step + 1) exit)
        let algoParts = 
            basicCfgDfsParts 
                preProcess
                processNode
                getNextNodes
                postProcess
        let visited, (step, enter, exit) = dfs algoParts cfg.EntryElement Set.empty initState
        enter, exit

    let findCycleFirstNodes enter exit =
        cfg.AllElements
        |> Seq.filter (fun e -> Map.containsKey e.Id enter)
        |> Seq.map
            (
                fun s -> 
                    if s <> null
                    then 
                        let srcEnter = Map.find s.Id enter
                        let srcExit = Map.find s.Id exit
                        s.Exits
                        |> Seq.choose 
                            (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
                        |> Seq.choose 
                            (
                                fun t ->
                                    let tarEnter = Map.find t.Id enter
                                    let tarExit = Map.find t.Id exit
                                    if tarEnter < srcEnter && srcExit < tarExit
                                    then Some(t.Id)
                                    else None
                            )
                    else Seq.empty
            )
        |> Seq.concat

    let enter, exit = calculateEnterExit cfg
    findCycleFirstNodes enter exit |> Set.ofSeq