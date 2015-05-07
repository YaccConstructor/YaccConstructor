module UserDefOperationInfo

open JetBrains.ReSharper.Psi.CSharp.Tree

type ArbitraryOperationInfo =
| CsharpArbitraryFun of IMethodDeclaration
| NoInfo

type ArbitraryOperation = {
    Name: string
    Info: ArbitraryOperationInfo}