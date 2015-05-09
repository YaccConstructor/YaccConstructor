module UserDefOperationInfo

open JetBrains.ReSharper.Psi.CSharp.Tree

open ResharperCsharpTreeUtils

type ArbitraryOperationInfo =
| CsharpArbitraryFun of IMethodDeclaration
| NoInfo

type ArbitraryOperation = {
    Name: string
    Info: ArbitraryOperationInfo }

module ArbitraryOperationFuns =
    let tryGetStringTypedParams { Name = _; Info = info } =
        match info with
        | CsharpArbitraryFun (methodDecl) ->
            Some(getStringTypedParams methodDecl)
        | NoInfo -> None