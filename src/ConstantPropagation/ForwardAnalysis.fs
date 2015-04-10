// Functions for control flow graph to automaton conversion
module ForwardAnalysis

open Microsoft.FSharp.Collections

open GenericGraphs
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Utils

type LoopInfo = {
    BodyNodes: Set<int>
    ExitNodes: Set<int> }

let collectLoopNodesInfo (ddg: DDG) =
    let wrongNodeTypeMsg = "wrong node type in collectLoopBodyNodes"
        
    let collectLoopBodyNodes (loopNode: GraphNode) =     
        let rec go (node: GraphNode) finalNodeId visited =
            if not <| Set.contains node visited
            then 
                let visited' = Set.add node visited
                match node.Type with
                | LoopNode(enterEdgeIndex, _) when node.Id <> finalNodeId ->
                    let loopEnterNode = ddg.Graph.InEdge(node, enterEdgeIndex).Source
                    go loopEnterNode finalNodeId visited'
                | LoopNode(_, _) -> visited'
                | _ -> 
                    ddg.Graph.InEdges(node) 
                    |> Seq.fold (fun v e -> go e.Source finalNodeId v) visited'
            else visited
        match loopNode.Type with 
        | LoopNode(_, bodyEdgeIndex) ->
            let bodyLastNode = ddg.Graph.InEdge(loopNode, bodyEdgeIndex).Source
            go bodyLastNode loopNode.Id Set.empty
        | _ -> failwith wrongNodeTypeMsg

    let collectLoopExitNodes (loopNode: GraphNode) (loopBodyNodes: Set<GraphNode>) = 
        let hasNonLoopSuccessors (node: GraphNode) =
            ddg.Graph.OutEdges(node)
            |> Seq.exists (fun e -> not <| Set.contains e.Target loopBodyNodes)
        let directExitNodes = Set.filter hasNonLoopSuccessors loopBodyNodes 
        let rec dfsUntilLoopNode (node: GraphNode) visited =         
            if not <| Set.contains node.Id visited
            then 
                let visited' = Set.add node.Id visited
                if node.Id <> loopNode.Id
                then 
                    ddg.Graph.InEdges(node) 
                    |> Seq.fold (fun acc e -> dfsUntilLoopNode e.Source acc) visited'
                else visited'
            else visited
        directExitNodes |> Seq.fold (fun v n -> dfsUntilLoopNode n v) Set.empty

    let collectInfo (loopNode: GraphNode) =
        let loopBodyNodes = collectLoopBodyNodes loopNode
        let loopExitNodesIds = collectLoopExitNodes loopNode loopBodyNodes
        let loopBodyNodesIds = loopBodyNodes |> Set.map (fun n -> n.Id)
        { BodyNodes = loopBodyNodesIds; ExitNodes = loopExitNodesIds }
 
    ddg.Graph.Vertices 
    |> Seq.filter (fun n -> match n.Type with LoopNode(_,_) -> true | _ -> false)
    |> Seq.map (fun n -> n.Id, collectInfo n)
    |> Map.ofSeq
            
type DirectionChecker = GraphNode -> LoopInfo -> bool
type LoopsDirectionHelper = {
    LoopsInfo: Map<int, LoopInfo>
    Stack: list<LoopInfo * DirectionChecker> }

module LoopsDirectionHelperFuncs =
    let private wrongNodeTypeMsg = "node type is wrong, LoopNode type expected"

    let private isLoopBodyNode (node: GraphNode) (loopInfo: LoopInfo) =
        Set.contains node.Id loopInfo.BodyNodes
    let private isLoopExitOrOuterNode (node: GraphNode) (loopInfo: LoopInfo) =
        let id = node.Id
        (not <| Set.contains id loopInfo.BodyNodes) || Set.contains id loopInfo.ExitNodes

    let private setDirection (node: GraphNode) (dirHelper: LoopsDirectionHelper) checker =
        match Map.tryFind node.Id dirHelper.LoopsInfo with
        | Some(loopInfo) -> 
            { dirHelper with Stack = (loopInfo, checker) :: dirHelper.Stack }
        | None -> failwith wrongNodeTypeMsg

    let create ddg = { 
        LoopsInfo = collectLoopNodesInfo ddg;
        Stack = [] }

    let setLoopBodyDirection node dirHelper = 
        setDirection node dirHelper isLoopBodyNode

    let setLoopExitDirection node dirHelper = 
        setDirection node dirHelper isLoopExitOrOuterNode

    let hasRestrictions (dirHelper: LoopsDirectionHelper) =
        not <| List.isEmpty dirHelper.Stack

    let isRightDirection node (dirHelper: LoopsDirectionHelper) =
        if hasRestrictions dirHelper
        then
            let loopInfo, checker = List.head dirHelper.Stack
            let isRightDir = checker node loopInfo
            let loopInfo' = { 
                BodyNodes = Set.remove node.Id loopInfo.BodyNodes;
                ExitNodes = Set.remove node.Id loopInfo.ExitNodes }
            let dirStack' =
                if Set.isEmpty loopInfo'.BodyNodes && Set.isEmpty loopInfo'.ExitNodes
                then { dirHelper with Stack = List.tail dirHelper.Stack }
                else { dirHelper with Stack = (loopInfo', checker) :: dirHelper.Stack }
            isRightDir, dirStack'
        else true, dirHelper

type State = char * Position<int>
type FSAMap = Map<string, FSA<State>>

let buildAutomaton (ddg: DDG) =
    // exception messages
    let unsupportedCaseMsg = "unsupported case encountered"
    let unexpectedNodeMsg = "unexpected node type is encountered in automaton building"
    let emptyAutomataMapsListMsg = "empty automataMaps list is passed to union function"
    let fixpointComputationProblemMsg = "old fsa map contains more variables than the new one in fixpoint computation"

    let unionTwoFsaMaps (a1: FSAMap) (a2: FSAMap) =
        let rec go (a1: list<string * FSA<State>>) (a2: FSAMap) (acc: FSAMap) =
            match a1 with
            | [] -> Map.fold (fun acc k v -> Map.add k v acc) acc a2
            | (varName, fsa1) :: tl ->
                let unionFsa =
                    match Map.tryFind varName a2 with
                    | Some(fsa2) -> FSA.Union (fsa1, fsa2)
                    | None -> fsa1
                let acc' = Map.add varName unionFsa acc
                let a2' = Map.remove varName a2
                go tl a2' acc'
        go (Map.toList a1) a2 Map.empty

    let fixpointAchieved (oldFsaMap: FSAMap) (widenedFsaMap: FSAMap) =
        let rec go (oldFsaList: list<string * FSA<State>>) (widenedFsaMap: FSAMap) =
            match oldFsaList with
            | [] -> true
            | (varName, oldFsa) :: tl ->
                match Map.tryFind varName widenedFsaMap with
                | Some(widenedFsa) -> 
                    if FsaHelper.isSubAutomaton widenedFsa oldFsa
                    then go tl widenedFsaMap
                    else false
                | None -> failwith fixpointComputationProblemMsg
        let oldIsEmpty = Map.isEmpty oldFsaMap
        let widenedIsEmpty = Map.isEmpty widenedFsaMap
        if oldIsEmpty && widenedIsEmpty
        then true
        elif not oldIsEmpty && not widenedIsEmpty
        then go (Map.toList oldFsaMap) widenedFsaMap
        else false

    let rec unionFsaMaps (automataMaps: list<FSAMap>) =
        match automataMaps with
        | [] -> failwith emptyAutomataMapsListMsg
        | a1 :: a2 :: tl -> 
            let unionFsaMap = unionTwoFsaMaps a1 a2
            unionFsaMaps (unionFsaMap :: tl)
        | a :: [] -> a

    let fsaForLiteral literal = 
        let initial = ResizeArray.singleton 0
        let final = ResizeArray.singleton 1
        // !back ref is int typed and set to 0 for now
        let transitionLabel = (literal, 0)
        let transitions = ResizeArray.singleton (0, transitionLabel, 1)
        let fsaApprox = Appr(initial, final, transitions)
        fsaApprox.ApprToFSA()

    let processAssingLikeNode target operands automata =
        let rightOperandFsa = List.head operands
        let operands' = List.tail operands
        let automata' = Map.add target rightOperandFsa automata
        operands', automata'

    let applyConcat operands =
        let concatRightOpFsa = List.head operands
        let concatLeftOpFsa = operands |> List.tail |> List.head
        let operands' = operands |> List.tail |> List.tail
        let concatResFsa = FSA.Concat (concatLeftOpFsa, concatRightOpFsa)
        concatResFsa :: operands'

    let processNodeByType
            (nodeType: GraphNodeType) 
            (operands: list<FSA<State>>) 
            (automata: FSAMap) =
        match nodeType with
        | Literal(value) ->
            let fsa = fsaForLiteral value
            let operands' = fsa :: operands
            operands', automata
        | VarRef(name) ->
            let fsaForVar = Map.find name automata
            let operands' = fsaForVar :: operands
            operands', automata
        | Declaration(name, _) ->
            processAssingLikeNode name operands automata
        | Updater(target, uType) ->
            let operands' = 
                match uType with
                | Assign(_) -> operands
                | PlusAssign(_, _) -> applyConcat operands
            processAssingLikeNode target operands' automata
        | Operation(oType, _) ->
            match oType with
            // here I use the assumption that Replace operation has 
            // the following operands order: Replace(originalStr, matchStr, replacementStr)
            // todo: add explicit order to Replace
            | Replace ->
                let replacementFsa = List.head operands
                let matchFsa = (List.tail >> List.head) operands
                let originalFsa = (List.tail >> List.tail >> List.head) operands
                let operands' = (List.tail >> List.tail >> List.tail) operands
                let replaceResFsa = FsaHelper.replace originalFsa matchFsa replacementFsa
                let operands' = replaceResFsa :: operands'
                operands', automata
            | Concat -> (applyConcat operands), automata
            | Arbitrary(name) -> failwith unsupportedCaseMsg
        | _ -> failwith unexpectedNodeMsg

    let rec build 
            (node: GraphNode) 
            (operands: list<FSA<State>>) 
            (automata: FSAMap) 
            (unionNodes: Map<GraphNode, list<FSAMap>>) =
        let processSuccessors node operands automata unionNodes =
            ddg.Graph.OutEdges(node)
            |> List.ofSeq
            |> List.map (fun e -> e.Target)
            |> List.fold (fun (_, un) succ -> build succ operands automata un) (automata, unionNodes)

        let processNodeAndSuccessors (node: GraphNode) operands automata unionNodes =
            let operands', automata' = processNodeByType node.Type operands automata
            processSuccessors node operands' automata' unionNodes

        let entriesNum = ddg.Graph.InEdges(node) |> List.ofSeq |> List.length
        let optTraversedEntries = Map.tryFind node unionNodes
        let traversedEntries = automata :: Option.getOrElse optTraversedEntries []
        let traversedNum = List.length traversedEntries
        let allEntriesTraversed =
            entriesNum = 0 ||
            match node.Type with
            | LoopNode(_, _) -> entriesNum - 1 = traversedNum // todo: support more than 2 entries
            | _ -> entriesNum = traversedNum
        if not allEntriesTraversed
        then
            let unionNodes' = Map.add node traversedEntries unionNodes
            automata, unionNodes'
        else
            // all paths leading to this node are traversed, we can union automata
            // and continue processing its successors
            let unionFsaMap = unionFsaMaps traversedEntries
            let unionNodes' = Map.remove node unionNodes
            match node.Type with
            | LoopNode(_, _) -> 
//                let loopFsa = Map.tryFind node loopsFsa
//                match loopFsa with
//                | Some(fsaMaps) -> 
//                    // we have already been in current loop node
//                    let oldFsaMap = List.head fsaMaps
//                    let unionFsaMap = unionTwoFsaMaps oldFsaMap automata
//                    // todo: next line is stub, widening operator required
//                    let widenedFsaMap = unionFsaMap
//                    if fixpointAchieved oldFsaMap widenedFsaMap 
//                    then
//                        let unionNodes' = Map.add node [] unionNodes
//                        processSuccessors node operands widenedFsaMap unionNodes'
//                        // exit loop processing
//                    else  
//                        let unionNodes' = Map.add node [widenedFsaMap] unionNodes
//                        // continue processing body
//                | None -> 
//                    // just start processing body
                failwith unsupportedCaseMsg
            | _ -> processNodeAndSuccessors node operands unionFsaMap unionNodes'
    
    let varFsaMap, _ = build ddg.Root [] Map.empty Map.empty
    let nfsa = Map.find ddg.VarName varFsaMap
    nfsa.NfaToDfa
