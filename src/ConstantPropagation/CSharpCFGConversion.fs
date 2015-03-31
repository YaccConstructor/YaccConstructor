module CSharpCFGConversion

open QuickGraph

open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree

open GenericCFG
open GenericCFGFuncs
open XMLParser
open Utils
open CSharpCFGInfo

open System.Collections.Generic

let private extractCSharpCfgInfo (cfgElem: IControlFlowElement) (info: CSharpCFGInfo) =
    let wrongForNodeStructureMsg = 
        @"For loop structure assumption failed - 
        node with more than 2 in edges encountered"

    let addAstCfgMapping (cfgElem: IControlFlowElement) (map': Map<int, Set<ControlFlowElemWrapper>>) =
        let node = cfgElem.SourceElement
        if node <> null
        then
            let nodeHash = hash node
            let curSet =
                match Map.tryFind nodeHash map' with
                | Some(value) -> value
                | None -> Set.empty
            let updSet = Set.add (ControlFlowElemWrapper(cfgElem)) curSet
            Map.add nodeHash updSet map'
        else
            map'

    /// Searches first node with 2 in edges. Throws exception if 
    /// node with more than 2 in edges encountered (due to assumptions 
    /// about For loops cfg structure)
    let rec traverseDown (node: IControlFlowElement) (prev: IControlFlowElement) =
        if node.Entries.Count > 2
        then failwith wrongForNodeStructureMsg
        elif node.Entries.Count = 2
        then
            let prevNodeEntryIndex = 
                node.Entries
                    |> List.ofSeq
                    |> List.findIndex (fun en -> en.Source = prev)
            node, prevNodeEntryIndex
        else traverseDown node.Exits.[0].Target node

    let processForStmt (cfgElem: IControlFlowElement) (loopNodes: Map<int, LoopNodeInfo>) 
                       (astCfgMap: Map<int, Set<ControlFlowElemWrapper>>) =
        let nodeHash = hash cfgElem.SourceElement
        match Map.tryFind nodeHash astCfgMap with
        | Some(cfgSet) when cfgSet.Count = 3 ->
            let enterForNode =
                cfgSet
                |> Array.ofSeq
                |> Array.sort
                |> fun arr -> Array.get arr 0
                |> fun w -> w.Value
            let forNodeChild = enterForNode.Exits.[0].Target
            let loopNode, enterEdgeIndex = traverseDown forNodeChild enterForNode
            let bodyExitEdgeIndex = (enterEdgeIndex + 1) % 2
            let loopNodeInfo = { EnterEdgeIndex = enterEdgeIndex; BodyExitEdgeIndex = bodyExitEdgeIndex }
            Map.add loopNode.Id loopNodeInfo loopNodes
        | _ -> loopNodes
    
    let astCfgMap' = addAstCfgMapping cfgElem info.AstCfgMap
    let loopNodes' =
        if cfgElem.SourceElement :? IForStatement
        then processForStmt cfgElem info.LoopNodes astCfgMap'
        else info.LoopNodes
    { info with AstCfgMap = astCfgMap'; LoopNodes = loopNodes' }
            
let collectAdditionalInfo (cfg: ICSharpControlFlowGraf) =
    let rec dfs (cfgElem: IControlFlowElement) (extractInfo: IControlFlowElement -> 'Info -> 'Info) 
                (visited: HashSet<int>) (info: 'Info) =
        if cfgElem <> null && (not << visited.Contains) cfgElem.Id
        then
            visited.Add cfgElem.Id |> ignore
            let info' = extractInfo cfgElem info
            cfgElem.Exits
            |> List.ofSeq
            |> List.map (fun rib -> rib.Target)
            |> List.fold (fun accInfo e -> dfs e extractInfo visited accInfo) info'
        else
            info 

    let visited = new HashSet<int>()
    dfs cfg.EntryElement extractCSharpCfgInfo visited emptyCSharpCfgInfo

let private multipleCfgNodesForAstNodeMsg = 
    "ast node maps to multiple cfg nodes where single mapping expected"

let private correspondingCfe (treeNode: ITreeNode) (cfgInfo: CSharpCFGInfo) =
    let cfgNodes = cfgInfo.AstCfgMap.[treeNode.GetHashCode()]
    if cfgNodes.Count > 1
    then failwith multipleCfgNodesForAstNodeMsg
    else cfgNodes |> List.ofSeq |> List.head

let convert (csharpCFG: ICSharpControlFlowGraf) (cfgInfo: CSharpCFGInfo) =
    // exception messages 
    let badIRefExprCastMsg = 
        "unable to perform cast to IReferenceExpression"
    let unexpectedInitializerTypeMsg =
        "unexpected initializer type in local variable declaration"

    let (|CSharpLoopNode|_|) (cfe: IControlFlowElement) =
        match Map.tryFind cfe.Id cfgInfo.LoopNodes with
        | Some(lnInfo) as i -> i
        | _ -> None

    let correspondingCfeId (treeNode: ITreeNode) = 
        let cfe = correspondingCfe treeNode cfgInfo
        cfe.Value.Id

    let isReplaceMethod (name: string) (callTargetType: IType) =
        name = "Replace" && callTargetType.IsString()

    let toGenericNode (cfe: IControlFlowElement) = 
        let nType= 
            match cfe with
            | CSharpLoopNode(info) -> 
                LoopNode(info.EnterEdgeIndex, info.BodyExitEdgeIndex)
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
                            Assign(correspondingCfeId initializer)
                        else 
                            let operands = assignExpr.OperatorOperands |> List.ofSeq
                            let fstOp = operands |> List.head
                            let sndOp = operands |> List.tail |> List.head
                            PlusAssign(correspondingCfeId fstOp, correspondingCfeId sndOp)
                    Updater(target, assingnType)
                | :? ILocalVariableDeclaration as locVarDecl ->
                    let name = locVarDecl.NameIdentifier.Name
                    let initializer = 
                        match locVarDecl.Initializer with
                        | :? IExpressionInitializer as re -> re.Value
                        | _ -> failwith unexpectedInitializerTypeMsg
                    Declaration(name, correspondingCfeId initializer)
                | :? ICSharpLiteralExpression as literalExpr ->
                    let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                    Literal(literalVal)
                // todo: add support for static methods
                | :? IInvocationExpression as invocExpr ->
                    let castToIRefExpr (n: ITreeNode) =
                        if n :? IReferenceExpression
                        then n :?> IReferenceExpression
                        else failwith badIRefExprCastMsg
                    let invokedExpr = castToIRefExpr invocExpr.InvokedExpression
                    let methodName = invokedExpr.NameIdentifier.Name
                    let callTargetRefExpr = castToIRefExpr invokedExpr.QualifierExpression
                    let declaredMethod = 
                        let invocExprRef = invocExpr.InvocationExpressionReference.Resolve()
                        invocExprRef.DeclaredElement :?> IMethod
                    let args = 
                        invocExpr.Arguments 
                        |> List.ofSeq
                        |> List.map (fun a -> a.Value)
                    let dependencies = 
                        if declaredMethod.IsStatic
                        then args
                        else callTargetRefExpr :> ICSharpExpression :: args
                    let depIDs = dependencies |> List.map correspondingCfeId
                    if isReplaceMethod methodName (callTargetRefExpr.Type())
                    then Operation(Replace, depIDs)
                    else Operation(Arbitrary(methodName), depIDs)
                | :? IReferenceExpression as refExpr ->
                    let name = refExpr.NameIdentifier.Name
                    VarRef(name)
                | :? IAdditiveExpression as addExpr -> 
                    let operandsIDs = 
                        addExpr.OperatorOperands 
                        |> List.ofSeq
                        |> List.map correspondingCfeId
                    Operation(Concat, operandsIDs) 
                | _ -> OtherNode
        { Id = cfe.Id; Type = nType }

    let rec dfs (cfe: IControlFlowElement) (visited: Set<int>) (genCFG: GenericCFG) =
        if Set.contains cfe.Id visited |> not
        then
            let visited' = Set.add cfe.Id visited
            let cur = toGenericNode cfe
            let children =
                cfe.Exits
                |> List.ofSeq
                |> List.choose (fun rib -> if rib.Target <> null then Some(rib.Target) else None) 
            children
            |> List.map (fun t -> toGenericNode t)
            |> List.iter(fun node -> genCFG.Graph.AddVerticesAndEdge(new Edge<CFGNode>(cur, node)) |> ignore)
            List.iter (fun ch -> dfs ch visited' genCFG) children

    let genericCFG = create()
    dfs csharpCFG.EntryElement Set.empty genericCFG
    genericCFG

let extractVarRefFromHotspot (hotInvocation: IInvocationExpression) (info: CSharpCFGInfo) (cfg: GenericCFG) =
    // here I use the first argument, but in general case
    // info from hotspot must be used
    let hotVarRef = hotInvocation.Arguments.[0].Value
    let cfe = correspondingCfe hotVarRef info
    cfg.Graph.Vertices
    |> List.ofSeq
    |> List.find (fun n -> n.Id = cfe.Value.Id)