/// Functions for building FSA from DDG
module GenerateFsa

open Microsoft.FSharp.Collections

open GenericGraphs
open DDG
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation
open Utils
open GraphUtils.TopoTraverser
open GenericGraphs.BidirectTopoDownTraverser

type FSAMap<'a when 'a : equality> = Map<string, FSA<'a>>

type BuildState<'a when 'a : equality> = {
    Operands: list<FSA<'a>>
    NodesToVarFsa: Map<int, FSAMap<'a>>
    UnboundResults: Map<int, FSA<'a>> }

/// Builds FSA for a given DDG.
let buildAutomaton (ddg: DDG<_,_>) (initialFsaMap: FSAMap<_>) controlData approximate (fsaParams: FsaParams<_,_>) =
    // exception messages
    let unsupportedCaseMsg = "unsupported case encountered"
    let unexpectedNodeMsg = "unexpected node type is encountered in automaton building"
    let emptyAutomataMapsListMsg = "empty automataMaps list is passed to union function"
    let fixpointComputationProblemMsg = "old fsa map contains more variables than the new one in fixpoint computation"
    let unexpectedPreExitMsg = "unexpected pre exit node type"
    let nonePrevNodeMsg = "loop node is achieved but prev node is None, ddg structure may be incorrect"
    let loopNodeMarkersMsg = "loop node may has incorrect markers"

    let mergeTwoFsaMasp merger (a1: FSAMap<_>) (a2: FSAMap<_>) =
        let rec go (a1: list<string * FSA<_>>) (a2: FSAMap<_>) (acc: FSAMap<_>) =
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

    let unionTwoFsaMaps (a1: FSAMap<_>) (a2: FSAMap<_>) =
        let union fsa1 fsa2 = FSA.Union (fsa1, fsa2)
        mergeTwoFsaMasp union a1 a2

    let widenTwoFsaMaps (a1: FSAMap<_>) (a2: FSAMap<_>) (fsaParams: FsaParams<_,_>) =
        let widenWithParams a1 a2 = FSA<_>.Widen (a1, a2, fsaParams)
        mergeTwoFsaMasp widenWithParams a1 a2 

    let fixpointAchieved (oldFsaMap: FSAMap<_>) (widenedFsaMap: FSAMap<_>) (fsaParams: FsaParams<_,_>) =
        let rec go (oldFsaList: list<string * FSA<_>>) (widenedFsaMap: FSAMap<_>) =
            match oldFsaList with
            | [] -> true
            | (varName, oldFsa) :: tl ->
                match Map.tryFind varName widenedFsaMap with
                | Some(widenedFsa) -> 
                    if FSA<_>.IsSubFsa (widenedFsa, oldFsa, fsaParams)
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

    let rec unionFsaMaps (automataMaps: list<FSAMap<_>>) =
        match automataMaps with
        | [] -> failwith emptyAutomataMapsListMsg
        | a1 :: a2 :: tl -> 
            let unionFsaMap = unionTwoFsaMaps a1 a2
            unionFsaMaps (unionFsaMap :: tl)
        | a :: [] -> a

    let fsaForLiteral literalValue literalNode = 
        let initial = ResizeArray.singleton 0
        let final = ResizeArray.singleton 1
        let transitionLabel = (literalValue, literalNode)
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
       
    let varsFsaCollectedSoFar node traverser (state: BuildState<_>) =
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

    let replace orig m r (fp: FsaParams<_,_>) =
        let args = (orig, m, r, fp.SeparatorSmbl1, fp.SeparatorSmbl2, fp.GetChar, fp.NewSymbol, fp.SymbolsAreEqual)
        FSA.Replace args

    let processNode (node: GraphNode<_,_>) (varsFsaSoFar: FSAMap<_>) traverser (state: BuildState<_>) =
        let operands, curNodeVarsFsa, unboundRes, traverser =
            match node.Type with
            | Literal(value, literalNode) ->
                let fsa = fsaForLiteral value literalNode
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
                    let replaceResFsa = replace originalFsa matchFsa replacementFsa fsaParams
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
                        | _ -> FSA<_>.CreateAnyWordsFsa fsaParams, state.Operands
                    let operands' = fsa :: operands
                    let unboundRes' = Map.add node.Id fsa state.UnboundResults
                    operands', varsFsaSoFar, unboundRes', traverser
            | ExitNode(nodes) ->
                let getPreExitNodeFsa (node: GraphNode<_,_>) =
                    match node.Type with
                    | VarRef(name) -> Map.find name varsFsaSoFar
                    | Operation(_) 
                    | Literal(_) -> Map.find node.Id state.UnboundResults
                    | _ -> failwith unexpectedPreExitMsg
                let exitFsa = nodes |> List.map getPreExitNodeFsa
                let unitedFsa = List.reduce (fun a1 a2 -> FSA.Union (a1, a2)) exitFsa
                let unboundRes' = Map.add node.Id unitedFsa state.UnboundResults
                state.Operands, varsFsaSoFar, unboundRes', traverser
            | LoopNode ->
                let prevVarsFsa = defaultArg (Map.tryFind node.Id state.NodesToVarFsa) Map.empty
                let unionFsaMap = unionTwoFsaMaps prevVarsFsa varsFsaSoFar
                let widenedFsaMap = widenTwoFsaMaps unionFsaMap prevVarsFsa fsaParams
                if fixpointAchieved prevVarsFsa widenedFsaMap fsaParams
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

    let rec propagate traverser (state: BuildState<_>) =
        let node, traverser = nextNode ddg.Graph traverser
        let varsFsaSoFar = varsFsaCollectedSoFar node traverser state
        let traverser, state = processNode node varsFsaSoFar traverser state
        match node.Type with
        | ExitNode(_) ->
            let result = Map.find node.Id state.UnboundResults
            result.NfaToDfa()
        | _ -> propagate traverser state

    let initState = {
        Operands = []
        NodesToVarFsa = Map.empty
        UnboundResults = Map.empty }
    let traverser = init ddg.Root
    let resultFsa = propagate traverser initState
    resultFsa