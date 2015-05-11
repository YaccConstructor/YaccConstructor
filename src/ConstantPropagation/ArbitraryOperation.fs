/// Types representing functions and methods regular 
/// approximation algorithm can be applied to
module ArbitraryOperation

open JetBrains.ReSharper.Psi.CSharp.Tree

open ResharperCsharpTreeUtils

/// Holds the language dependent data necessary to build the 
/// approximation of method or function
type ArbitraryOperationInfo =
| CsharpArbitraryFun of IMethodDeclaration
| NoInfo

/// Represents any function or method
type ArbitraryOperation = {
    Name: string
    Info: ArbitraryOperationInfo }

module ArbitraryOperationFuns =
    let tryGetStringTypedParams { Name = _; Info = info } =
        match info with
        | CsharpArbitraryFun (methodDecl) ->
            Some(getStringTypedParams methodDecl)
        | NoInfo -> None