// Functions for control flow graph to automaton conversion
module ForwardAnalysis

open Microsoft.FSharp.Collections

open GenericCFG
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation
open Utils

type State = char * Position<int>
type FSAMap = Map<string, FSA<State>>

let buildAutomaton (cfg: GenericCFG) =
    // exception messages
    let unsupportedCaseMsg = "unsupported case encountered"
    let unexpectedNodeMsg = "unexpected node type is encountered in automaton building"
    let emptyAutomataMapsListMsg = "empty automataMaps list is passed to union function"

    let unionTwoFsaMaps (a1: FSAMap) (a2: FSAMap) =
        let rec go (a1: list<string * FSA<State>>) (a2: FSAMap) (acc: FSAMap) =
            match a1 with
            | [] -> acc
            | (varName, fsa1) :: tl ->
                let unionFsa =
                    match Map.tryFind varName a2 with
                    | Some(fsa2) -> FSA.Union (fsa1, fsa2)
                    | None -> fsa1
                let acc' = Map.add varName unionFsa acc
                go tl a2 acc'
        go (Map.toList a1) a2 Map.empty

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
            (nodeType: CFGNodeType) 
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
            (node: CFGNode) 
            (operands: list<FSA<State>>) 
            (automata: FSAMap) 
            (unionNodes: Map<CFGNode, list<FSAMap>>) =
        let processNodeAndSuccessors 
                (node: CFGNode) 
                (operands: list<FSA<State>>) 
                (automata: FSAMap) 
                (unionNodes: Map<CFGNode, list<FSAMap>>) =
            let operands', automata' = processNodeByType node.Type operands automata
            cfg.Graph.OutEdges(node)
            |> List.ofSeq
            |> List.map (fun e -> e.Target)
            |> List.fold (fun (_, un) succ -> build succ operands' automata' un) (automata', unionNodes)

        let entriesNum = cfg.Graph.InEdges(node) |> List.ofSeq |> List.length
        if entriesNum > 1
        // union or loop node processing
        then
            match node.Type with
            | LoopNode(_, _) -> failwith unsupportedCaseMsg
            | _ ->
                let automataMaps = 
                    let optMaps = Map.tryFind node unionNodes
                    Option.getOrElse optMaps []
                let automataMaps' = automata :: automataMaps
                if List.length automataMaps' < entriesNum
                then
                    // not all entry paths are traversed yet
                    let unionNodes' = Map.add node automataMaps' unionNodes
                    automata, unionNodes'
                else
                    // all paths leading to this node are traversed, we can union automata
                    // and continue processing its successors
                    let unionMap = unionFsaMaps automataMaps'
                    let unionNodes' = Map.add node [] unionNodes
                    processNodeAndSuccessors node operands unionMap unionNodes'
        else
            // sequential node processing
            processNodeAndSuccessors node operands automata unionNodes
    
    let varFsaMap, _ = build cfg.Root [] Map.empty Map.empty
    let finalVarName = 
        match cfg.Final.Type with 
        | VarRef(name) -> name
        | _ -> failwith "CFG has wrong typed final node"
    let nfsa = Map.find finalVarName varFsaMap
    nfsa.NfaToDfa
