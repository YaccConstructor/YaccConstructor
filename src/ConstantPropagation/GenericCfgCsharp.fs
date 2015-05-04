module GenericCfgCsharp

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

open System.Collections.Generic

open QuickGraph

open GenericGraphs
open IControlFlowGraphUtils
open Utils.DictionaryFuns
            
type AstToCfgDict = Dictionary<ITreeNode, HashSet<IControlFlowElement>>
type AstToGenericNodesDict = Dictionary<ITreeNode, HashSet<GraphNode>>

type LoopNodeInfo = {
    LoopExit: IControlFlowElement
    BodyEnter: IControlFlowElement
    BodyConditionNodes: HashSet<IControlFlowElement>
    ExitConditionNodes: HashSet<IControlFlowElement>
    BodyExits: list<IControlFlowRib> }

type ConvertInfo = {
    AstToCfgMapping: AstToCfgDict
    LoopNodes: Dictionary<IControlFlowElement, LoopNodeInfo>
    AstToGenericNodesMapping: AstToGenericNodesDict
    CfeToGenericNodesMapping: Dictionary<IControlFlowElement, GraphNode> }

// convert info collecting functions
let private createAstNodeToCfeDict (cfg: IControlFlowGraf) =
    let astNodeToCfeDict = AstToCfgDict()
    let processNode (e: IControlFlowElement) () =
        if e <> null && e.SourceElement <> null
        then do addToSetInDict e.SourceElement e astNodeToCfeDict
    do dfsCfgExits cfg.EntryElement processNode () |> ignore
    astNodeToCfeDict

let private findLoopConditionExits (cfg: IControlFlowGraf) (astNodeToCfeDict: AstToCfgDict) =
    let getForElems (astNodeToCfeDict: AstToCfgDict) =
        astNodeToCfeDict 
        |> Seq.choose 
            (
                fun kvp -> 
                    if kvp.Key :? IForStatement 
                    then 
                        let forNode = kvp.Key :?> IForStatement
                        kvp.Value 
                        |> Seq.sortBy (fun e -> e.Id) 
                        |> Seq.head
                        |> fun e -> Some(forNode, e)
                    else None
            )

    let loopNodeToConditionMapping (preLoopElems: seq<IForStatement * IControlFlowElement>) loopNodes =
        let rec findLoopNode (elem: IControlFlowElement) =
            if Set.contains elem.Id loopNodes
            then elem
            else findLoopNode (elem.Exits |> Seq.head |> (fun edge -> edge.Target))
        preLoopElems
        |> Seq.map (fun (node, elem) -> node.Condition :> ITreeNode, findLoopNode elem)

    let conditionToExitsMapping (conditionToLoopNode: seq<ITreeNode * IControlFlowElement>) =
        // create
        let rawConditionsDict = 
            let dict = Dictionary()
            do conditionToLoopNode 
            |> Seq.map (fun (cond, _) -> cond, HashSet<IControlFlowElement>()) 
            |> Seq.iter (fun (c, s) -> dict.[c] <- s)
            dict
        // fill
        let processNode (e: IControlFlowElement) () =
            if e <> null && e.SourceElement <> null && rawConditionsDict.ContainsKey e.SourceElement
            then do addToSetInDict e.SourceElement e rawConditionsDict
        do dfsCfgExits cfg.EntryElement processNode () |> ignore
        // check and normalize
        let conditionsDict = Dictionary()
        do rawConditionsDict
        |> Seq.iter 
            (
                fun kvp ->
                    let condElemsNum = kvp.Value.Count
                    if condElemsNum < 2 then failwith "cond elems assumption failed"
                    let condElems = 
                        kvp.Value 
                        |> List.ofSeq 
                        |> List.sortBy (fun e -> e.Id) 
                        |> Seq.skip (condElemsNum - 2)
                    let exits = {
                        BodyEnter = Seq.head <| condElems; 
                        LoopExit = (Seq.head << Seq.skip 1) condElems; 
                        BodyConditionNodes = HashSet();
                        ExitConditionNodes = HashSet();
                        BodyExits = [] }
                    conditionsDict.[kvp.Key] <- exits
            )
        conditionsDict

    let forElems = getForElems astNodeToCfeDict
    let loopNodes = findLoopNodes cfg
    let conditionToLoopNode = loopNodeToConditionMapping forElems loopNodes
    let conditionToExitsDict = conditionToExitsMapping conditionToLoopNode
    // merge in one dict
    let loopsDict = Dictionary()
    conditionToLoopNode
    |> Seq.iter (fun (condNode, loopId) -> loopsDict.[loopId] <- conditionToExitsDict.[condNode])
    loopsDict

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

let private collectConditionNodes loopNodeId (loopInfo: LoopNodeInfo) =
    let processNode startId (e: IControlFlowElement) condNodes = 
        if e.Id <> loopNodeId && e.Id <> startId
        then e :: condNodes
        else condNodes
    let getNextNodes (e: IControlFlowElement) s =
        if e.Id = loopNodeId
        then [], s
        else getCfeEntries e s
    let bodyCondNodes =
        let processNode = processNode loopInfo.BodyEnter.Id
        snd <| dfsCfg loopInfo.BodyEnter processNode getNextNodes []
    let exitCondNodes =
        let processNode = processNode loopInfo.LoopExit.Id
        snd <| dfsCfg loopInfo.LoopExit processNode getNextNodes []
    HashSet(bodyCondNodes), HashSet(exitCondNodes)

let private collectLoopInfo cfg astNodeToCfeDict =
    let rawLoopsDict = findLoopConditionExits cfg astNodeToCfeDict
    let loopInfoUpdated = 
        rawLoopsDict 
        |> Seq.map
            (
                fun kvp ->
                    let loopNode = kvp.Key
                    let info = kvp.Value
                    let bodyCondNodes, exitCondNodes = collectConditionNodes loopNode.Id info
                    let bodyExits = findLoopBodyExits loopNode.Id info.BodyEnter
                    let info = 
                        { info with 
                            BodyConditionNodes = bodyCondNodes; 
                            ExitConditionNodes = exitCondNodes; 
                            BodyExits = bodyExits }
                    loopNode, info
            )
    dictFromSeq loopInfoUpdated

let collectConvertInfo (cfg: IControlFlowGraf) = 
    let astNodeToCfeDict = createAstNodeToCfeDict cfg
    let loopsDict = collectLoopInfo cfg astNodeToCfeDict
    { AstToCfgMapping = astNodeToCfeDict; 
    LoopNodes = loopsDict; 
    AstToGenericNodesMapping = Dictionary();
    CfeToGenericNodesMapping = Dictionary() }
    
// converting functions
let rec toGenericCfg (cfg: ICSharpControlFlowGraf) functionName =
    // exception messages 
    let astToGenericNodeMappingProblemMsg = 
        "ast node maps to multiple or zero generic nodes where single mapping expected" 
    let badIRefExprCastMsg = 
        "unable to perform cast to IReferenceExpression"
    let unexpectedInitializerTypeMsg =
        "unexpected initializer type in local variable declaration"
    // utility methods
    let (|CSharpLoopNode|_|) (info: ConvertInfo) (cfe: IControlFlowElement) =
        match info.LoopNodes.TryGetValue cfe with
        | true, exits -> Some(exits)
        | _ -> None
    let getGenericNodeId (treeNode: ITreeNode) (info: ConvertInfo) = 
        let res = getMappingToOne treeNode info.AstToGenericNodesMapping
        res.Id
    let isReplaceMethod (name: string) (callTargetType: IType) =
        name = "Replace" && callTargetType.IsString()

    let tryGetMethodDeclaration (invocExpr: IInvocationExpression) = 
        let decls = invocExpr
                        .InvocationExpressionReference
                        .Resolve()
                        .DeclaredElement
                        .GetDeclarations()
        if Seq.length decls <> 0
        then Some(decls |> Seq.head :?> IMethodDeclaration)
        else None
        
    let tryCreateLazyCfgGenerator (invocExpr: IInvocationExpression) =
        match tryGetMethodDeclaration invocExpr with
        | Some(methodDecl) ->
            let lazyGenerator () =
                let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
                let methodName = methodDecl.NameIdentifier.Name
                toGenericCfg csharpCfg methodName |> fst
            Some(lazyGenerator)
        | _ -> None

    let toGenericNode (cfe: IControlFlowElement) nodeId (info: ConvertInfo) = 
        let nType = 
            match cfe with
            | CSharpLoopNode info _ -> LoopNode
            | _ -> 
                match cfe.SourceElement with
                | :? IAssignmentExpression as assignExpr 
                    when (assignExpr.Dest :? IReferenceExpression)
                    && (assignExpr.AssignmentType = AssignmentType.EQ
                    || assignExpr.AssignmentType = AssignmentType.PLUSEQ)
                    ->
                    let target = (assignExpr.Dest :?> IReferenceExpression).NameIdentifier.Name 
                    let assingnType = 
                        if assignExpr.AssignmentType = AssignmentType.EQ
                        then
                            let initializer = 
                                assignExpr.OperatorOperands 
                                |> List.ofSeq 
                                |> List.tail 
                                |> List.head
                            Assign(getGenericNodeId initializer info)
                        else 
                            let operands = assignExpr.OperatorOperands |> List.ofSeq
                            let fstOp = operands |> List.head
                            let sndOp = operands |> List.tail |> List.head
                            PlusAssign(getGenericNodeId fstOp info, getGenericNodeId sndOp info)
                    Updater(target, assingnType)
                | :? ILocalVariableDeclaration as locVarDecl ->
                    let name = locVarDecl.NameIdentifier.Name
                    let initializer = 
                        match locVarDecl.Initializer with
                        | :? IExpressionInitializer as re -> re.Value
                        | _ -> failwith unexpectedInitializerTypeMsg
                    Declaration(name, getGenericNodeId initializer info)
                | :? ICSharpLiteralExpression as literalExpr ->
                    let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                    Literal(literalVal)
                | :? IInvocationExpression as invocExpr ->
                    let castToIRefExpr (n: ITreeNode) =
                        if n :? IReferenceExpression
                        then n :?> IReferenceExpression
                        else failwith badIRefExprCastMsg
                    let invokedExpr = castToIRefExpr invocExpr.InvokedExpression
                    let methodName = invokedExpr.NameIdentifier.Name
                    let callTargetRefExpr = castToIRefExpr invokedExpr.QualifierExpression
                    let psiMethod = 
                        invocExpr
                            .InvocationExpressionReference
                            .Resolve()
                            .DeclaredElement :?> IMethod
                    let args = 
                        invocExpr.Arguments 
                        |> List.ofSeq
                        |> List.map (fun a -> a.Value)
                    let dependencies = 
                        if psiMethod.IsStatic
                        then args
                        else callTargetRefExpr :> ICSharpExpression :: args
                    let depIDs = dependencies |> List.map (fun d -> getGenericNodeId d info)
                    if isReplaceMethod methodName (callTargetRefExpr.Type())
                    then Operation(Replace, depIDs)
                    else
                        let optGen = tryCreateLazyCfgGenerator invocExpr 
                        Operation(Arbitrary(optGen), depIDs)
                | :? IReferenceExpression as refExpr ->
                    let name = refExpr.NameIdentifier.Name
                    VarRef(name)
                | :? IAdditiveExpression as addExpr -> 
                    let operandsIDs = 
                        addExpr.OperatorOperands 
                        |> List.ofSeq
                        |> List.map (fun op -> getGenericNodeId op info)
                    Operation(Concat, operandsIDs)
                | :? IReturnStatement as retStmt ->
                    // node: only var refs are supported in return statement for now
                    let varRef = retStmt.Value :?> IReferenceExpression
                    VarRef(varRef.NameIdentifier.Name)
                | _ -> OtherNode
        { Id = nodeId; Type = nType }

    let connectToTraversedSuccessors (e: IControlFlowElement) currentNode (graph: BidirectGraph) (info: ConvertInfo) =
        let tryGetGenericOfTarget (rib: IControlFlowRib)=
            if rib.Target <> null 
            then 
                match info.CfeToGenericNodesMapping.TryGetValue rib.Target with
                | true, s -> Some(s)
                | false, _ -> None
            else None
        e.Exits 
        |> Seq.choose tryGetGenericOfTarget
        |> Seq.iter (fun succ -> graph.AddEdge (Edge(currentNode, succ)) |> ignore)

    let processNode 
            (e: IControlFlowElement) 
            (graph: BidirectGraph, parentsStack, lastId, info: ConvertInfo, alternativeExits) =
        // convert cfe to generic node
        let newId = lastId + 1
        let genericNode = toGenericNode e newId info
        // update info
        if e.SourceElement <> null
        then do addToSetInDict e.SourceElement genericNode info.AstToGenericNodesMapping
        do info.CfeToGenericNodesMapping.[e] <- genericNode
        // add to graph
        if List.isEmpty parentsStack
        then do graph.AddVertex genericNode |> ignore
        else do graph.AddVerticesAndEdge (Edge(List.head parentsStack, genericNode)) |> ignore
        // check if there are already traversed successors
        // we must connect current node to
        do connectToTraversedSuccessors e genericNode graph info
        // if cur node is loop, we must continue processing from it's exits
        // so alternative exits are added and will be used in getNextNodes during dfs
        let alternativeExits =
            match e with
            | CSharpLoopNode info loopInfo ->
                loopInfo.BodyEnter :: loopInfo.LoopExit :: alternativeExits
            | _ -> alternativeExits
        graph, genericNode :: parentsStack, newId, info, alternativeExits

    let getNextNodes (e: IControlFlowElement) ((g, ps, li, i, alternativeExits) as state) =
        if List.isEmpty alternativeExits
        then getCfeExits e state
        else alternativeExits, (g, ps, li, i, [])

    let postProcess e (g, parentsStack, li, i, ae) =
        (g, List.tail parentsStack, li, i, ae)

    let surroundLoopsWithMarkers (graph: BidirectGraph) (info: ConvertInfo) lastId =
        let addOutMarkNode (edges: seq<Edge<GraphNode>>) markNode =
            do edges |> Seq.iter (graph.RemoveEdge >> ignore)
            let src = edges |> Seq.head |> (fun e -> e.Source)
            do Edge(src, markNode) |> graph.AddVerticesAndEdge |> ignore
            let markToTargets = edges |> Seq.map (fun e -> Edge(markNode, e.Target))
            do graph.AddVerticesAndEdgeRange markToTargets |> ignore
        let addInMarkNode (edges: seq<Edge<GraphNode>>) markNode =
            do edges |> Seq.iter (graph.RemoveEdge >> ignore)
            let dst = edges |> Seq.head |> (fun e -> e.Target)
            do Edge(markNode, dst) |> graph.AddVerticesAndEdge |> ignore
            let sourcesToMark = edges |> Seq.map (fun e -> Edge(e.Source, markNode))
            do graph.AddVerticesAndEdgeRange sourcesToMark |> ignore
        let getLoopGenericInEdges (loopNode: GraphNode) (bodyExits: list<IControlFlowRib>) =
            let nodes = 
                bodyExits 
                |> List.map (fun r -> info.CfeToGenericNodesMapping.[r.Source])
                |> Set.ofList
            graph.InEdges(loopNode) 
            |> List.ofSeq
            |> List.partition (fun r -> Set.contains r.Source nodes)
        let getLoopGenericOutEdges (loopNode: GraphNode) bodyEnter =
            graph.OutEdges(loopNode)
            |> List.ofSeq
            |> List.partition (fun r -> info.CfeToGenericNodesMapping.[bodyEnter] = r.Target)
        let surroundLoopWithMarkers (loopNode: GraphNode) (loopInfo: LoopNodeInfo) lastId =
            let genericBodyExits, genericLoopEntries = getLoopGenericInEdges loopNode loopInfo.BodyExits
            let newId = lastId + 1
            let bodyEnd = { Id = newId; Type = LoopBodyEnd }
            do addInMarkNode genericBodyExits bodyEnd
            let newId = newId + 1
            let loopEnter = { Id = newId; Type = LoopEnter }
            do addInMarkNode genericLoopEntries loopEnter
            let genericBodyEnter, genericLoopExits = getLoopGenericOutEdges loopNode loopInfo.BodyEnter
            let newId = newId + 1
            let bodyBeg = { Id = newId; Type = LoopBodyBeg }
            do addOutMarkNode genericBodyEnter bodyBeg
            let newId = newId + 1
            let loopExit = { Id = newId; Type = LoopExit }
            do addOutMarkNode genericLoopExits loopExit
            newId

        (lastId, info.LoopNodes)
        ||> Seq.fold 
            (
                fun prevId (KeyValue(loopCfe, loopInfo)) -> 
                    let loopNode = info.CfeToGenericNodesMapping.[loopCfe]
                    surroundLoopWithMarkers loopNode loopInfo prevId
            )

    let traverseConverting (cfg: ICSharpControlFlowGraf) (info: ConvertInfo) =
        let initState = (BidirectGraphFuns.create (), [], 0, info, [])
        let _, (graph, _, lastId, _, _) =
            dfsCfgBasic cfg.EntryElement (fun _ s -> s) processNode getNextNodes postProcess initState
        let lastId = surroundLoopsWithMarkers graph info lastId
        { FunctionName = functionName; Graph = graph; MaxNodeId = lastId }

    let convInfo = collectConvertInfo cfg 
    let genericCfg = traverseConverting cfg convInfo
    genericCfg, convInfo