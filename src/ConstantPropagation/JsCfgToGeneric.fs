module JsCfgToGeneric

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi.JavaScript.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Parsing
open JetBrains.ReSharper.Psi.JavaScript.Resolve

open ReshrperCfgToGeneric
open Utils
open GenericGraphs
open ReshrperCsharpTreeUtils
open UserDefOperationInfo

// exception messages
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
let isReplaceMethod (name: string) = 
    name = "replace"

let toGenericNode (cfe: IControlFlowElement) nodeId (info: ConvertInfo) = 
    let nType = 
        match cfe with
        | LoopCfe info _ -> LoopNode
        | _ -> 
            match cfe.SourceElement with
            | :? IBinaryExpression as assignExpr 
                when assignExpr.IsAssignment
                && (assignExpr.Sign.GetTokenType() = JavaScriptTokenType.EQ
                || assignExpr.Sign.GetTokenType() = JavaScriptTokenType.PLUSEQ)
                ->
                let assignTokenType = assignExpr.Sign.GetTokenType()
                let target = (assignExpr.LeftOperand :?> IReferenceExpression).Name
                let assingnType = 
                    if assignTokenType = JavaScriptTokenType.EQ
                    then Assign(getGenericNodeId assignExpr.RightOperand info)
                    else
                        let fstOp = assignExpr.LeftOperand
                        let sndOp = assignExpr.RightOperand
                        PlusAssign(getGenericNodeId fstOp info, getGenericNodeId sndOp info)
                Updater(target, assingnType)
            | :? IVariableDeclaration as varDecl ->
                let name = varDecl.DeclaredName
                Declaration(name, getGenericNodeId varDecl.Value info)
            | :? IJavaScriptLiteralExpression as literalExpr ->
                let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                Literal(literalVal)
            | :? IInvocationExpression as invocExpr ->
                let invokedExpr = invocExpr.InvokedExpression :?> IReferenceExpression
                let methodName = invokedExpr.Name
                let callTargetRefExpr = invokedExpr.Qualifier
                let args = 
                    let argsList = List.ofSeq invocExpr.Arguments
                    if callTargetRefExpr <> null
                    then callTargetRefExpr :: argsList
                    else argsList
                let depIDs = args |> List.map (fun a -> getGenericNodeId a info)
                if isReplaceMethod methodName
                then Operation(Replace, depIDs)
                else Operation(Arbitrary(NoInfo), depIDs)
            | :? IReferenceExpression as refExpr ->
                let name = refExpr.Name
                VarRef(name)
            | :? IBinaryExpression as addExpr 
                when addExpr.Sign.GetTokenType() = JavaScriptTokenType.PLUS
                -> 
                let operandsIDs = 
                    [addExpr.LeftOperand; addExpr.RightOperand]
                    |> List.map (fun op -> getGenericNodeId op info) 
                Operation(Concat, operandsIDs) 
            | :? IReturnStatement as retStmt ->
                // node: only var refs are supported in return statement for now
                let varRef = retStmt.Value :?> IReferenceExpression
                VarRef(varRef.Name)
            | _ -> OtherNode
    { Id = nodeId; Type = nType }

let private tryAsLoopTreeNode (node: ITreeNode) =
    match node with
    | :? IForStatement as forStmt -> Some(forStmt.ForCondition :> ITreeNode)
    | _ -> None

let rec toGenericCfg (cfg: IJsControlFlowGraf) functionName =
    ReshrperCfgToGeneric.toGenericCfg 
        cfg 
        toGenericNode 
        tryAsLoopTreeNode 
        (flip (|LoopCfe|_|)) 
        functionName