module CFGConversionJs

open CSharpCFGInfo
open Utils
open GenericCFG
open CfgUtils

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi.JavaScript.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Parsing
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.JavaScript.Resolve

let private extractJsCfgInfo (cfgElem: IControlFlowElement) (info: CSharpCFGInfo) =   
    let astCfgMap' = addAstCfgMapping cfgElem info.AstCfgMap
    { info with AstCfgMap = astCfgMap' }
            
let collectAdditionalInfo (cfg: IJsControlFlowGraf) =
    collectAdditionalInfo cfg extractJsCfgInfo emptyCSharpCfgInfo

let convert (csharpCFG: IJsControlFlowGraf) (cfgInfo: CSharpCFGInfo) =
    let correspondingCfeId (treeNode: ITreeNode) = 
        let cfe = correspondingCfe treeNode cfgInfo
        cfe.Value.Id

    let isReplaceMethod (name: string) = 
        name = "replace"

    let toGenericNode (cfe: IControlFlowElement) = 
        let nType= 
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
                    then
                        Assign(correspondingCfeId assignExpr.RightOperand)
                    else
                        let fstOp = assignExpr.LeftOperand
                        let sndOp = assignExpr.RightOperand
                        PlusAssign(correspondingCfeId fstOp, correspondingCfeId sndOp)
                Updater(target, assingnType)
            | :? IVariableDeclaration as varDecl ->
                let name = varDecl.DeclaredName
                Declaration(name, correspondingCfeId varDecl.Value)
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
                let depIDs = List.map correspondingCfeId args
                if isReplaceMethod methodName
                then Operation(Replace, depIDs)
                else Operation(Arbitrary(methodName), depIDs)
            | :? IReferenceExpression as refExpr ->
                let name = refExpr.Name
                VarRef(name)
            | :? IBinaryExpression as addExpr 
                when addExpr.Sign.GetTokenType() = JavaScriptTokenType.PLUS
                -> 
                let operandsIDs = 
                    [addExpr.LeftOperand; addExpr.RightOperand]
                    |> List.map correspondingCfeId
                Operation(Concat, operandsIDs) 
            | _ -> OtherNode
        { Id = cfe.Id; Type = nType }

    CfgUtils.convert csharpCFG toGenericNode

let extractVarRefFromHotspot (hotVarRef: IJavaScriptExpression) (info: CSharpCFGInfo) (cfg: GenericCFG) =
    let cfe = correspondingCfe hotVarRef info
    cfg.Graph.Vertices
    |> List.ofSeq
    |> List.find (fun n -> n.Id = cfe.Value.Id)