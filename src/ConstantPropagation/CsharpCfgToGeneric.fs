module CsharpCfgToGeneric

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow

open ReshrperCfgToGeneric
open Utils
open GenericGraphs
open ReshrperCsharpTreeUtils
open UserDefOperationInfo

// exception messages 
let private astToGenericNodeMappingProblemMsg = 
    "ast node maps to multiple or zero generic nodes where single mapping expected" 
let private badIRefExprCastMsg = 
    "unable to perform cast to IReferenceExpression"
let private unexpectedInitializerTypeMsg =
    "unexpected initializer type in local variable declaration"

// utility methods
let (|LoopCfe|_|) (info: ConvertInfo) (cfe: IControlFlowElement) =
    match info.LoopNodes.TryGetValue cfe with
    | true, loopInfo -> Some(loopInfo)
    | _ -> None
let getGenericNodeId (treeNode: ITreeNode) (info: ConvertInfo) = 
    let res = DictionaryFuns.getMappingToOne treeNode info.AstToGenericNodes
    res.Id
let isReplaceMethod (name: string) (callTargetType: IType) =
    name = "Replace" && callTargetType.IsString()

let toGenericNode (cfe: IControlFlowElement) nodeId (info: ConvertInfo) = 
    let nType = 
        match cfe with
        | LoopCfe info _ -> LoopNode
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
                    if psiMethod.IsStatic || not <| callTargetRefExpr.Type().IsString()
                    then args
                    else callTargetRefExpr :> ICSharpExpression :: args
                let depIDs = dependencies |> List.map (fun d -> getGenericNodeId d info)
                if isReplaceMethod methodName (callTargetRefExpr.Type())
                then Operation(Replace, depIDs)
                else
                    let operationInfo =
                        match tryGetMethodDeclaration invocExpr with
                        | Some(methodDecl) -> CsharpArbitraryFun(methodDecl)
                        | _ -> NoInfo
                    Operation(Arbitrary(operationInfo), depIDs)
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

let private tryAsLoopTreeNode (node: ITreeNode) =
    match node with
    | :? IForStatement as forStmt -> Some(forStmt.Condition :> ITreeNode)
    | _ -> None
    
// converting functions
let rec toGenericCfg (cfg: ICSharpControlFlowGraf) functionName =
    ReshrperCfgToGeneric.toGenericCfg 
        cfg 
        toGenericNode 
        tryAsLoopTreeNode 
        (flip (|LoopCfe|_|)) 
        functionName