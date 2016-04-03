/// Utilities for JetBrains.ReSharper.Psi.CSharp.Tree
module ResharperCsharpTreeUtils

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp.ControlFlow

open System.IO

open IControlFlowGraphUtils

/// Returns ICSharpControlFlowGraph graph for ITreeNode
let nodeToCSharpCfg (declaration : ITreeNode) = 
    let cSharpCfgBuilder = new CSharpControlFlowBuilder()
    let graph = cSharpCfgBuilder.GraphFromNode (declaration, null, true)
    graph :?> ICSharpControlFlowGraph

/// Extracts C# CFG from method declaration and converts it
/// to DOT's digraph. The output file's name and the digraph's name
/// are the same as passed method's name
let methodCfgToDot (methodDecl: IMethodDeclaration) (outDirPath: string) =
    let methodName = methodDecl.NameIdentifier.GetText()
    let outPath = Path.Combine(outDirPath, methodName + ".dot")
    let cfg = nodeToCSharpCfg methodDecl
    cfgToDot cfg outPath methodName

/// Creates CFGs for all the methods in the passed file and outputs
/// CFGs to the folder specified by "outDirPath" in DOT format.
let allMethodsCfgToDot (file: ICSharpFile) (outDirPath: string)=
    let processorAction (node: ITreeNode) = 
        match node with
        | :? IMethodDeclaration as methodDecl -> methodCfgToDot methodDecl outDirPath
        | _ -> ()
    let processor = RecursiveElementProcessor (fun node -> processorAction node)
    processor.Process file

/// Returns the names of passed method's string typed parameners
let getStringTypedParams (methodDecl: IMethodDeclaration) =
    methodDecl.Params.ParameterDeclarations
    |> Seq.choose 
        (fun p -> if p.Type.IsString() then Some(p.DeclaredName) else None)

/// Tries to extract IMethodDeclaration from passed IInvocationExpression
let tryGetMethodDeclaration (invocExpr: IInvocationExpression) = 
    let decls = invocExpr
                    .InvocationExpressionReference
                    .Resolve()
                    .DeclaredElement
                    .GetDeclarations()
    if Seq.length decls <> 0
    then Some(decls |> Seq.head :?> IMethodDeclaration)
    else None

/// Extracts IMethod from passed IInvocationExpression
let getPsiMethod (invocExpr: IInvocationExpression) =
    invocExpr.InvocationExpressionReference.Resolve().DeclaredElement :?> IMethod

/// Returns name, class name, IParameter array and return type as IType
/// for passed IInvocationExpression
let getMethodSigniture (invocExpr: IInvocationExpression) =
    let psiMethod = getPsiMethod invocExpr
    let containingType = psiMethod.GetContainingType()
    let className = containingType.ShortName
    let methodName = psiMethod.ShortName
    let parameters = psiMethod.Parameters |> Array.ofSeq
    let returnType = psiMethod.ReturnType
    methodName, className, parameters, returnType

let private getEnclosingMethodNullParentMsg = 
    "can't get enclosing method, null parent encountered"
/// Returns the method enclosing passed ITreeNode by traversing it's parents
let rec getEnclosingMethod (node: ITreeNode) =
    match node with
    | null -> failwith getEnclosingMethodNullParentMsg
    | :? IMethodDeclaration as funDecl -> funDecl
    | _ -> getEnclosingMethod node.Parent