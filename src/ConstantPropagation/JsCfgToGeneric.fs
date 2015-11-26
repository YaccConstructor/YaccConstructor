/// JavaScript specific functions needed to run IControlFlowGraph to generic CFG
/// conversion algo
module JsCfgToGeneric

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi.JavaScript.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Parsing
open JetBrains.ReSharper.Psi.JavaScript.Resolve

open ResharperCfgToGeneric
open Utils
open GenericGraphs
open ResharperCsharpTreeUtils
open ArbitraryOperation

// exception messages
let private badIRefExprCastMsg = 
    "unable to perform cast to IReferenceExpression"
let private unexpectedInitializerTypeMsg =
    "unexpected initializer type in local variable declaration"

// utility methods
let private (|LoopCfe|_|) (info: ConvertInfo<_,_>) (cfe: IControlFlowElement) =
    match info.LoopNodes.TryGetValue cfe with
    | true, loopInfo -> Some(loopInfo)
    | _ -> None
let private getGenericNodeId (treeNode: ITreeNode) (info: ConvertInfo<_,_>) = 
    let res = Dictionary.getMappingToOne treeNode info.AstToGenericNodes
    res.Id
let private isReplaceMethod (name: string) = 
    name = "replace"

let private toGenericNode (cfe: IControlFlowElement) nodeId (info: ConvertInfo<_,_>) = 
    let nType = 
        match cfe with
        | LoopCfe info _ -> LoopNode
        | _ -> 
            match cfe.SourceElement with
            | :? IBinaryExpression as assignExpr 
                when (*assignExpr.IsAssignment
                &&*) (assignExpr.Sign.GetTokenType() = JavaScriptTokenType.EQ
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
                let name = varDecl.NameNode.GetDeclaredName()//was varDecl.DeclaredName
                Declaration(name, getGenericNodeId varDecl.Value info)
            | :? IJavaScriptLiteralExpression as literalExpr ->
                let literalVal = literalExpr.Literal.GetText().Trim[|'\"'|]
                Literal(literalVal, literalExpr)
            | :? IInvocationExpression as invocExpr ->
                let invokedExpr = invocExpr.InvokedExpression :?> IReferenceExpression
                let methodName = invokedExpr.Name
                let callTargetRefExpr = invokedExpr.Qualifier :> IExpressionOrSpread
                let args = 
                    let argsList = List.ofSeq invocExpr.Arguments
                    if callTargetRefExpr <> null
                    then callTargetRefExpr :: argsList
                    else argsList
                let depIDs = args |> List.map (fun a -> getGenericNodeId a info)
                if isReplaceMethod methodName
                then Operation(Replace, depIDs)
                else Operation(Arbitrary(None), depIDs)
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

/// Converts JavaScript IJsControlFlowGraph to generic CFG by calling generic conversion
/// algo with JavaScript specific functions passed as arguments
let rec toGenericCfg (cfg: IJsControlFlowGraph) functionName =
    ResharperCfgToGeneric.toGenericCfg 
        cfg 
        toGenericNode 
        JsLoopInfo.findLoopConditionExits 
        (flip (|LoopCfe|_|)) 
        functionName