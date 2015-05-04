module BuildApproximation

open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree

open GenericCfgCsharp
open GenericGraphs
open FsaHelper
open GenerateFsa

let bindArgsToParams (methodDecl: IMethodDeclaration) (stack: list<CharFSA>) =
    let parameters = 
        methodDecl.Params.ParameterDeclarations 
        |> Seq.map (fun p -> p.DeclaredName)
    let paramsNum = Seq.length parameters
    if List.length stack < paramsNum 
    then failwith "stack contains too few elements"
    else
        let argsToBind = Seq.take paramsNum stack |> List.ofSeq |> List.rev
        let restStack = Seq.skip paramsNum stack |> List.ofSeq
        let boundParams = Seq.zip parameters argsToBind
        Map.ofSeq boundParams, restStack

let rec fsaGenerator (functionInfo: ArbitraryOperationInfo) (stack: list<CharFSA>) recLevel =
    if recLevel - 1 < 0
    then None, stack
    else 
        match functionInfo with
        | NoInfo -> None, stack
        | CsharpArbitraryFun (methodDecl) ->
            let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
            let methodName = methodDecl.NameIdentifier.Name
            let genericCFG, _ = toGenericCfg csharpCfg methodName
            let ddg = GenericCFGFuncs.ddgForExits genericCFG
            let initFsaMap, restStack = bindArgsToParams methodDecl stack
            let fsa = buildAutomaton ddg initFsaMap (recLevel - 1) fsaGenerator
            // for debug
            let path = Utils.myDebugFilePath ("fsa_" + methodName + ".dot")
            FsaHelper.toDot fsa path
            // end
            Some(fsa), restStack
        | _ -> failwith "wrong operation info type"