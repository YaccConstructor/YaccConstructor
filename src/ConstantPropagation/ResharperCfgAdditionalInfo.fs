/// Additional info for JetBrains.ReSharper.Psi.ControlFlow.
/// The info includes mapping from AST elements (from JetBrains.ReSharper.Psi.Tree)
/// to JetBrains.ReSharper.Psi.ControlFlow.IControlFlowElements
/// (back mapping is provided by Resharper), and information about loops
/// in IControlFlowGraph. All this info is used to convert language specific
/// IControlFlowGraph to Generic CFG.
module ResharperCfgAdditionalInfo

open System.Collections.Generic

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow

open Utils
open GraphUtils
open IControlFlowGraphUtils

type AstToCfgDict = Dictionary<ITreeNode, HashSet<IControlFlowElement>>
type LoopToConditionExitsDict = Dictionary<IControlFlowElement, IControlFlowElement*IControlFlowElement>

module GeneralCfgInfoFuns =
    /// Builds mapping from AST elements to IControlFlowElements
    /// for a given IControlFlowGraph
    let astNodeToCfeDict (cfg: IControlFlowGraph) =
        let astNodeToCfeDict = AstToCfgDict()
        let processNode (e: IControlFlowElement) () =
            if e <> null && e.SourceElement <> null
            then do Dictionary.addToSetInDict e.SourceElement e astNodeToCfeDict
        let algoParts = cfgExitsDfsParts processNode
        do dfs algoParts cfg.EntryElement Set.empty () |> ignore
        astNodeToCfeDict

/// Holds the info about a loop in IControlFlowGraph. The loop has the following structure.
/// The first node of a loop is called loop node. The loop node's subtree (part of it, the nearest
/// to the node) represents loop's condition. This subtree part ends with 2 exit nodes - one
/// leads to the loop's body (hold by BodyEnter field), another - out of the loop (hold by 
/// LoopExit field). All the nodes that can be achieved from BodyEnter node before loop node are
/// stored in BodyConditionNodes, from LoopExit node before loop node - in ExitConditionNodes (these
/// 2 sets can intersect). Also edges from last loop body's nodes to loop node are stored in BodyExits
type LoopNodeInfo = {
    BodyEnter: IControlFlowElement
    LoopExit: IControlFlowElement
    BodyConditionNodes: HashSet<IControlFlowElement>
    ExitConditionNodes: HashSet<IControlFlowElement>
    BodyExits: list<IControlFlowEdge> }

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
        let algoParts = cfgDfsParts processNode getNextNodes
        snd <| dfs algoParts bodyEnterNode Set.empty []

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
            let algoParts = cfgDfsParts processNode getNextNodes
            snd <| dfs algoParts bodyEnter Set.empty []
        let exitCondNodes =
            let processNode = processNode loopExit.Id
            let algoParts = cfgDfsParts processNode getNextNodes
            snd <| dfs algoParts loopExit Set.empty []
        HashSet(bodyCondNodes), HashSet(exitCondNodes)

    /// Collects the info about loops, producing the mapping from each 
    /// loop node to it's info. The second argument of a function is a 
    /// mapping from loop nodes to BodyEnter and LoopExit nodes pair
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
        |> Dictionary.dictFromSeq