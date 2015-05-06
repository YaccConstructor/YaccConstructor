module ResharperCsharpTreeUtils

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree

let getStringTypedParams (methodDecl: IMethodDeclaration) =
    methodDecl.Params.ParameterDeclarations
    |> Seq.choose 
        (fun p -> if p.Type.IsString() then Some(p.DeclaredName) else None)


let tryGetMethodDeclaration (invocExpr: IInvocationExpression) = 
    let decls = invocExpr
                    .InvocationExpressionReference
                    .Resolve()
                    .DeclaredElement
                    .GetDeclarations()
    if Seq.length decls <> 0
    then Some(decls |> Seq.head :?> IMethodDeclaration)
    else None

let getPsiMethod (invocExpr: IInvocationExpression) =
    invocExpr.InvocationExpressionReference.Resolve().DeclaredElement :?> IMethod

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
let rec getEnclosingMethod (node: ITreeNode) =
    match node with
    | null -> failwith getEnclosingMethodNullParentMsg
    | :? IMethodDeclaration as funDecl -> funDecl
    | _ -> getEnclosingMethod node.Parent