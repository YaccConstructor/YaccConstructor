/// Utils for C# source code approximation
module CsharpApprxomationUtils

open JetBrains.ReSharper.Psi.CSharp.Tree

open ResharperCsharpTreeUtils
open QuickGraph.FSA.GraphBasedFsa

/// Binds passed method's string typed parameners to top elements of given stack.
/// If method has N string typed parameters top N elements will be poped and 
/// bound to parameters in reversed order, in other words the topmost stack element
/// is bound to the last parameter. Returns the Map from parameter names to stack 
/// elements and the rest of the stack
let bindArgsToParams (methodDecl: IMethodDeclaration) (stack: list<FSA<_>>) =
    let parameters = getStringTypedParams methodDecl
    let paramsNum = Seq.length parameters
    if List.length stack < paramsNum 
    then failwith "stack contains too few elements"
    else
        let argsToBind = Seq.take paramsNum stack |> List.ofSeq |> List.rev
        let restStack = Seq.skip paramsNum stack |> List.ofSeq
        let boundParams = Seq.zip parameters argsToBind
        Map.ofSeq boundParams, restStack