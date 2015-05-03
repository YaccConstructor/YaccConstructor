module IControlFlowGraphUtils

open JetBrains.ReSharper.Psi.ControlFlow

open GraphUtils

let dfsCfgBasic node preProcess processNode getNextNodes postProcess state = 
    let isVisited (e: IControlFlowElement) v = Set.contains e.Id v
    let makeVisited (e: IControlFlowElement) v = Set.add e.Id v
    let initVisited = Set.empty
    dfs node isVisited makeVisited initVisited preProcess processNode getNextNodes postProcess state

let dfsCfg node processNode getNextNodes state =
    dfsCfgBasic node (fun e s -> s) processNode getNextNodes (fun e s -> s) state

let getCfeExits (e: IControlFlowElement) s =
    let exits =
        e.Exits
        |> Seq.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None)
        |> List.ofSeq
    exits, s

let getCfeEntries (e: IControlFlowElement) s =
    let entries =
        e.Entries
        |> Seq.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None)
        |> List.ofSeq
    entries, s

let dfsCfgExits node processNode state =
    dfsCfg node processNode getCfeExits state

let dfsCfgEntries node processNode state =
    let getEntries (e: IControlFlowElement) s =
        let entries =
            e.Entries
            |> Seq.choose (fun rib -> if rib.Source <> null then Some(rib.Source) else None)
            |> List.ofSeq
        entries, s
    dfsCfg node processNode getEntries state
 
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
        let visited, (step, enter, exit) = 
            dfsCfgBasic cfg.EntryElement preProcess processNode getNextNodes postProcess initState
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