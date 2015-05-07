module ResharperCfgAdditionalInfo

open System.Collections.Generic

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow

open Utils
open IControlFlowGraphUtils

type AstToCfgDict = Dictionary<ITreeNode, HashSet<IControlFlowElement>>
type LoopToConditionExitsDict = Dictionary<IControlFlowElement, IControlFlowElement*IControlFlowElement>

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

    let collect cfg (loopNodeToConditionExits: LoopToConditionExitsDict) =
        let collectInfo (loopNode: IControlFlowElement) bodyEnter loopExit =
            let bodyCondNodes, exitCondNodes = collectConditionNodes loopNode.Id bodyEnter loopExit
            let bodyExits = findLoopBodyExits loopNode.Id bodyEnter
            {   BodyEnter = bodyEnter
                LoopExit = loopExit
                BodyConditionNodes = bodyCondNodes
                ExitConditionNodes = exitCondNodes
                BodyExits = bodyExits }
        loopNodeToConditionExits 
        |> Seq.map 
            (fun (KeyValue(loopCfe, (bodyEnter, loopExit))) -> 
                loopCfe, collectInfo loopCfe bodyEnter loopExit)
        |> DictionaryFuns.dictFromSeq