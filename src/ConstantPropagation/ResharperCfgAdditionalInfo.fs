module ResharperCfgAdditionalInfo

open System.Collections.Generic

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow

open Utils
open IControlFlowGraphUtils

type AstToCfgDict = Dictionary<ITreeNode, HashSet<IControlFlowElement>>

module GeneralCfgInfoFuns =
    let astNodeToCfeDict (cfg: IControlFlowGraf) =
        let astNodeToCfeDict = AstToCfgDict()
        let processNode (e: IControlFlowElement) () =
            if e <> null && e.SourceElement <> null
            then do DictionaryFuns.addToSetInDict e.SourceElement e astNodeToCfeDict
        do dfsCfgExits cfg.EntryElement processNode () |> ignore
        astNodeToCfeDict

type LoopNodeInfo = {
    BodyEnter: IControlFlowElement
    LoopExit: IControlFlowElement
    BodyConditionNodes: HashSet<IControlFlowElement>
    ExitConditionNodes: HashSet<IControlFlowElement>
    BodyExits: list<IControlFlowRib> }

module LoopNodeInfoFuns =
    let private findLoopConditionExits (cfg: IControlFlowGraf) tryAsLoopTreeNode (astNodeToCfeDict: AstToCfgDict) =
        let astConditionToPreLoopCfe =
            let loopChooser (node: ITreeNode) (cfeSet: HashSet<IControlFlowElement>) =
                match tryAsLoopTreeNode node with
                | Some(condition) ->
                    cfeSet 
                    |> Seq.sortBy (fun e -> e.Id) 
                    |> Seq.head
                    |> fun e -> Some(condition, e)
                | _ -> None
            astNodeToCfeDict 
            |> Seq.choose (fun (KeyValue(key, value)) -> loopChooser key value)
            |> DictionaryFuns.dictFromSeq
        let astConditionToExits =
            let astConditionToCfeDict = Dictionary<ITreeNode, HashSet<IControlFlowElement>>()
            let processNode (e: IControlFlowElement) () =
                if e <> null && e.SourceElement <> null 
                && astConditionToPreLoopCfe.ContainsKey e.SourceElement
                then do DictionaryFuns.addToSetInDict e.SourceElement e astConditionToCfeDict
            do dfsCfgExits cfg.EntryElement processNode () |> ignore
            let extractConditionExits (cfeSet: HashSet<IControlFlowElement>) =
                let setSize = cfeSet.Count
                if setSize < 2 then failwith "cond elems assumption failed"
                let condElems = 
                    cfeSet 
                    |> List.ofSeq 
                    |> List.sortBy (fun e -> e.Id) 
                    |> Seq.skip (setSize - 2)
                let bodyEnter = Seq.head <| condElems
                let loopExit = (Seq.head << Seq.skip 1) condElems
                (bodyEnter, loopExit)
            let astConditionToExitsDict = Dictionary()
            do astConditionToCfeDict
            |> Seq.iter 
                (fun (KeyValue(key, value)) -> 
                    astConditionToExitsDict.[key] <- extractConditionExits value)
            astConditionToExitsDict
        let preLoopKeyToLoopKey (preLoopToExits: Dictionary<IControlFlowElement, 'Exits>) loopNodes =
            let rec findLoopNode (elem: IControlFlowElement) =
                if Set.contains elem.Id loopNodes
                then elem
                else findLoopNode (elem.Exits |> Seq.head |> (fun edge -> edge.Target))
            preLoopToExits
            |> Seq.map (fun (KeyValue(preLoopNode, exits)) -> findLoopNode preLoopNode, exits)
            |> DictionaryFuns.dictFromSeq
        let preLoopToExits = DictionaryFuns.mergeDicts astConditionToPreLoopCfe astConditionToExits
        let loopNodes = findLoopNodes cfg
        preLoopKeyToLoopKey preLoopToExits loopNodes

    let private findLoopBodyExits loopNodeId bodyEnterNode =
        let processNode (e: IControlFlowElement) bodyExits =
            let curBodyExits = 
                e.Exits 
                |> Seq.filter (fun rib -> rib.Target <> null && rib.Target.Id = loopNodeId)
                |> List.ofSeq
            curBodyExits @ bodyExits
        let getNextNodes (e: IControlFlowElement) s =
            if e.Id = loopNodeId
            then [], s
            else getCfeExits e s
        snd <| dfsCfg bodyEnterNode processNode getNextNodes []

    let private collectConditionNodes loopNodeId (bodyEnter: IControlFlowElement) (loopExit: IControlFlowElement) =
        let processNode startId (e: IControlFlowElement) condNodes = 
            if e.Id <> loopNodeId && e.Id <> startId
            then e :: condNodes
            else condNodes
        let getNextNodes (e: IControlFlowElement) s =
            if e.Id = loopNodeId
            then [], s
            else getCfeEntries e s
        let bodyCondNodes =
            let processNode = processNode bodyEnter.Id
            snd <| dfsCfg bodyEnter processNode getNextNodes []
        let exitCondNodes =
            let processNode = processNode loopExit.Id
            snd <| dfsCfg loopExit processNode getNextNodes []
        HashSet(bodyCondNodes), HashSet(exitCondNodes)
    let collect cfg tryAsLoopTreeNode astNodeToCfeDict =
        let loopNodeToCondExits = findLoopConditionExits cfg tryAsLoopTreeNode astNodeToCfeDict
        let collectInfo (loopNode: IControlFlowElement) bodyEnter loopExit =
            let bodyCondNodes, exitCondNodes = collectConditionNodes loopNode.Id bodyEnter loopExit
            let bodyExits = findLoopBodyExits loopNode.Id bodyEnter
            {   BodyEnter = bodyEnter
                LoopExit = loopExit
                BodyConditionNodes = bodyCondNodes
                ExitConditionNodes = exitCondNodes
                BodyExits = bodyExits }
        loopNodeToCondExits 
        |> Seq.map 
            (fun (KeyValue(loopCfe, (bodyEnter, loopExit))) -> 
                loopCfe, collectInfo loopCfe bodyEnter loopExit)
        |> DictionaryFuns.dictFromSeq