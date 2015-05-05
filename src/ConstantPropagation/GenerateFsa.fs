module GenerateFsa

open Microsoft.FSharp.Collections

open GenericGraphs
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Utils

type FsaState = char * Position<int>
type FSAMap = Map<string, FsaHelper.CharFSA>

type BuildState = {
    Operands: list<FSA<FsaState>>
    NodesToVarFsa: Map<int, FSAMap>
    UnboundResults: Map<int, FSA<FsaState>> }

open GraphUtils.TopoTraverser
open GenericGraphs.CfgTopoDownTraverser

let buildAutomaton (ddg: DDG) (initialFsaMap: FSAMap) controlData approximate =
    // exception messages
    let unsupportedCaseMsg = "unsupported case encountered"
    let unexpectedNodeMsg = "unexpected node type is encountered in automaton building"
    let emptyAutomataMapsListMsg = "empty automataMaps list is passed to union function"
    let fixpointComputationProblemMsg = "old fsa map contains more variables than the new one in fixpoint computation"
    let unexpectedPreExitMsg = "unexpected pre exit node type"
    let nonePrevNodeMsg = "loop node is achieved but prev node is None, ddg structure may be incorrect"
    let loopNodeMarkersMsg = "loop node may has incorrect markers"

    let mergeTwoFsaMasp merger (a1: FSAMap) (a2: FSAMap) =
        let rec go (a1: list<string * FSA<FsaState>>) (a2: FSAMap) (acc: FSAMap) =
            match a1 with
            | [] -> Map.fold (fun acc k v -> Map.add k v acc) acc a2
            | (varName, fsa1) :: tl ->
                let unionFsa =
                    match Map.tryFind varName a2 with
                    | Some(fsa2) -> merger fsa1 fsa2
                    | None -> fsa1
                let acc' = Map.add varName unionFsa acc
                let a2' = Map.remove varName a2
                go tl a2' acc'
        go (Map.toList a1) a2 Map.empty

    let unionTwoFsaMaps (a1: FSAMap) (a2: FSAMap) =
        mergeTwoFsaMasp FsaHelper.union a1 a2

    let widenTwoFsaMaps (a1: FSAMap) (a2: FSAMap) =
        mergeTwoFsaMasp FsaHelper.widen a1 a2

    let fixpointAchieved (oldFsaMap: FSAMap) (widenedFsaMap: FSAMap) =
        let rec go (oldFsaList: list<string * FSA<FsaState>>) (widenedFsaMap: FSAMap) =
            match oldFsaList with
            | [] -> true
            | (varName, oldFsa) :: tl ->
                match Map.tryFind varName widenedFsaMap with
                | Some(widenedFsa) -> 
                    if FsaHelper.isSubFsa widenedFsa oldFsa
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
       
    let varsFsaCollectedSoFar node traverser (state: BuildState) =
        let inNodes =
            match node.Type with
            | LoopNode ->
                match prevNode traverser with
                | None -> failwith nonePrevNodeMsg
                | Some(prev) -> [prev]
            | _ ->
                ddg.Graph.InEdges node 
                |> Seq.map (fun e -> e.Source)
                |> List.ofSeq
        let varsFsaList = 
            if List.isEmpty inNodes 
            then [initialFsaMap] 
            else inNodes |> List.map (fun n -> Map.find n.Id state.NodesToVarFsa)
        unionFsaMaps varsFsaList

    let processNode (node: GraphNode) (varsFsaSoFar: FSAMap) traverser (state: BuildState) =
        let operands, curNodeVarsFsa, unboundRes, traverser =
            match node.Type with
            | Literal(value) ->
                let fsa = fsaForLiteral value
                let operands' = fsa :: state.Operands
                let unboundRes' = Map.add node.Id fsa state.UnboundResults
                operands', varsFsaSoFar, unboundRes', traverser
            | VarRef(name) ->
                let fsaForVar = Map.find name varsFsaSoFar
                let operands' = fsaForVar :: state.Operands
                operands', varsFsaSoFar, state.UnboundResults, traverser
            | Declaration(name, _) ->
                let ops', varsFsa' = processAssingLikeNode name state.Operands varsFsaSoFar
                ops', varsFsa', state.UnboundResults, traverser
            | Updater(target, uType) ->
                let operands = 
                    match uType with
                    | Assign(_) -> state.Operands
                    | PlusAssign(_, _) -> applyConcat state.Operands
                let ops', varsFsa' = processAssingLikeNode target operands varsFsaSoFar
                ops', varsFsa', state.UnboundResults, traverser
            | Operation(oType, _) ->
                match oType with
                // here I use the assumption that Replace operation has 
                // the following operands order: Replace(originalStr, matchStr, replacementStr)
                // todo: add explicit order to Replace
                | Replace ->
                    let replacementFsa = List.head state.Operands
                    let matchFsa = (List.tail >> List.head) state.Operands
                    let originalFsa = (List.tail >> List.tail >> List.head) state.Operands
                    let operands = (List.tail >> List.tail >> List.tail) state.Operands
                    let replaceResFsa = FsaHelper.replace originalFsa matchFsa replacementFsa
                    let operands' = replaceResFsa :: operands
                    let unboundRes' = Map.add node.Id replaceResFsa state.UnboundResults
                    operands', varsFsaSoFar, unboundRes', traverser
                | Concat -> 
                    let operands' = (applyConcat state.Operands)
                    let unboundRes' = Map.add node.Id (List.head operands') state.UnboundResults
                    operands', varsFsaSoFar, unboundRes', traverser
                | Arbitrary(info) -> 
                    let fsa, operands = 
                        match approximate info state.Operands controlData with
                        | Some(fsa), ops -> fsa, ops
                        | _ -> FsaHelper.anyWordsFsa (), state.Operands
                    let operands' = fsa :: operands
                    let unboundRes' = Map.add node.Id fsa state.UnboundResults
                    operands', varsFsaSoFar, unboundRes', traverser
            | ExitNode(nodes) ->
                let getPreExitNodeFsa (node: GraphNode) =
                    match node.Type with
                    | VarRef(name) -> Map.find name varsFsaSoFar
                    | Operation(_) 
                    | Literal(_) -> Map.find node.Id state.UnboundResults
                    | _ -> failwith unexpectedPreExitMsg
                let exitFsa = nodes |> List.map getPreExitNodeFsa
                let unitedFsa = List.reduce FsaHelper.union exitFsa
                let unboundRes' = Map.add node.Id unitedFsa state.UnboundResults
                state.Operands, varsFsaSoFar, unboundRes', traverser
            | LoopNode ->
                let prevVarsFsa = defaultArg (Map.tryFind node.Id state.NodesToVarFsa) Map.empty
                let unionFsaMap = unionTwoFsaMaps prevVarsFsa varsFsaSoFar
                let widenedFsaMap = widenTwoFsaMaps unionFsaMap prevVarsFsa
                if fixpointAchieved prevVarsFsa widenedFsaMap
                then 
                    let traverser' = setLoopExitDirection traverser
                    state.Operands, widenedFsaMap, state.UnboundResults, traverser'
                else 
                    let traverser' = setLoopBodyDirection traverser
                    state.Operands, widenedFsaMap, state.UnboundResults, traverser'
            | LoopEnter
            | LoopExit
            | LoopBodyBeg
            | LoopBodyEnd
            | StartNode -> state.Operands, varsFsaSoFar, state.UnboundResults, traverser
            | _ -> failwith unexpectedNodeMsg
        let state = {
            Operands = operands
            NodesToVarFsa = Map.add node.Id curNodeVarsFsa state.NodesToVarFsa
            UnboundResults = unboundRes }
        traverser, state

    let rec propagate traverser (state: BuildState) =
        let node, traverser = nextNode ddg.Graph traverser
        let varsFsaSoFar = varsFsaCollectedSoFar node traverser state
        let traverser, state = processNode node varsFsaSoFar traverser state
        match node.Type with
        | ExitNode(_) ->
            let result = Map.find node.Id state.UnboundResults
            result.NfaToDfa
        | _ -> propagate traverser state

    let initState = {
        Operands = []
        NodesToVarFsa = Map.empty
        UnboundResults = Map.empty }
    let traverser = init ddg.Root
    let resultFsa = propagate traverser initState
    resultFsa