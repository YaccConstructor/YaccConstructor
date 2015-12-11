/// C# specific functions needed to run IControlFlowGraph to generic CFG
/// conversion algo
module CsharpCfgToGeneric

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp.ControlFlow
open JetBrains.ReSharper.Psi.ControlFlow

open ResharperCfgToGeneric
open Utils
open GenericGraphs
open ResharperCsharpTreeUtils
open ArbitraryOperation
open ResharperCfgAdditionalInfo

let private (|LoopCfe|_|) (info: ConvertInfo<_,_>) (cfe: IControlFlowElement) =
    match info.LoopNodes.TryGetValue cfe with
    | true, loopInfo -> Some(loopInfo)
    | _ -> None

let private getGenericNodeId (treeNode: ITreeNode) (info: ConvertInfo<_,_>) = 
    let res = Dictionary.getMappingToOne treeNode info.AstToGenericNodes
    res.Id

let private isReplaceMethod (name: string) (callTargetType: IType) =
    name = "Replace" && callTargetType.IsString()

let private badIRefExprCastMsg = 
    "unable to perform cast to IReferenceExpression"
let private unexpectedInitializerTypeMsg =
    "unexpected initializer type in local variable declaration"
let private tryExtractNodeTypeInfo (node: ITreeNode) (info: ConvertInfo<_,_>) =
    match node with
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
        Some(Updater(target, assingnType))
    | :? ILocalVariableDeclaration as locVarDecl ->
        let name = locVarDecl.NameIdentifier.Name
        let initializer = 
            match locVarDecl.Initializer with
            | :? IExpressionInitializer as re -> re.Value
            | _ -> failwith unexpectedInitializerTypeMsg
        Some(Declaration(name, getGenericNodeId initializer info))
    | :? ICSharpLiteralExpression as literalExpr ->
        let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
        Some(Literal(literalVal, literalExpr))
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
        then Some(Operation(Replace, depIDs))
        else
            let operation = 
                tryGetMethodDeclaration invocExpr 
                |> Option.bind (fun methDecl -> Some { Name = methodName; Info = methDecl })
            Some(Operation(Arbitrary(operation), depIDs))
    | :? IReferenceExpression as refExpr ->
        let name = refExpr.NameIdentifier.Name
        Some(VarRef(name))
    | :? IAdditiveExpression as addExpr -> 
        let operandsIDs = 
            addExpr.OperatorOperands 
            |> List.ofSeq
            |> List.map (fun op -> getGenericNodeId op info)
        Some(Operation(Concat, operandsIDs))
    | _ -> None

let private returnStmtExpressionMsg = "unsupported expression in return statement is met"
let private toGenericNode (cfe: IControlFlowElement) nodeId (info: ConvertInfo<_,_>) = 
    let nType = 
        match cfe with
        | LoopCfe info _ -> LoopNode
        | _ -> 
            match tryExtractNodeTypeInfo cfe.SourceElement info with
            | Some(nodeType) -> nodeType
            | None -> 
                match cfe.SourceElement with
                | :? IReturnStatement as retStmt ->
                    match tryExtractNodeTypeInfo retStmt.Value info with
                    | Some(nodeType) -> nodeType
                    | None -> failwith returnStmtExpressionMsg
                | _ -> OtherNode
    { Id = nodeId; Type = nType }
    
/// Converts C# ICSharpControlFlowGraph to generic CFG by calling generic conversion
/// algo with C# specific functions passed as arguments
let rec toGenericCfg (cfg: ICSharpControlFlowGraph) functionName =
    ResharperCfgToGeneric.toGenericCfg 
        cfg 
        toGenericNode 
        CsharpLoopInfo.findLoopConditionExits 
        (flip (|LoopCfe|_|)) 
        functionName