module BuildApproximation

open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree

open UserDefOperationInfo
open GenericCfgCsharp
open GenericGraphs
open FsaHelper
open GenerateFsa
open Utils.DictionaryFuns
open ReshrperTreeUtils

type ControlData = {
    TargetMethod: string
    TargetNode: ITreeNode
    CurRecLevel: int }

let bindArgsToParams (methodDecl: IMethodDeclaration) (stack: list<CharFSA>) =
    let parameters = getStringTypedParams methodDecl
    let paramsNum = Seq.length parameters
    if List.length stack < paramsNum 
    then failwith "stack contains too few elements"
    else
        let argsToBind = Seq.take paramsNum stack |> List.ofSeq |> List.rev
        let restStack = Seq.skip paramsNum stack |> List.ofSeq
        let boundParams = Seq.zip parameters argsToBind
        Map.ofSeq boundParams, restStack

let rec approximate (functionInfo: ArbitraryOperationInfo) (stack: list<CharFSA>) (controlData: ControlData) =
    if controlData.CurRecLevel - 1 < 0
    then None, stack
    else 
        match functionInfo with
        | NoInfo -> None, stack
        | CsharpArbitraryFun (methodDecl) ->
            let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
            let methodName = methodDecl.NameIdentifier.Name
            let genericCFG, convertInfo = toGenericCfg csharpCfg methodName
            // for debug
            let path = Utils.myDebugFilePath ("cfg_" + methodName + ".dot")
            BidirectGraphFuns.toDot genericCFG.Graph methodName path
            // end
            let ddg =
                if methodName = controlData.TargetMethod
                then 
                    let targetNode = getMappingToOne controlData.TargetNode convertInfo.AstToGenericNodesMapping
                    GenericCFGFuncs.ddgForVar targetNode genericCFG
                else GenericCFGFuncs.ddgForExits genericCFG
            // for debug
            let path = Utils.myDebugFilePath ("ddg_" + methodName + ".dot")
            BidirectGraphFuns.toDot ddg.Graph methodName path
            // end
            let initFsaMap, restStack = bindArgsToParams methodDecl stack
            let controlData = { controlData with CurRecLevel = controlData.CurRecLevel - 1 }
            let fsa = buildAutomaton ddg initFsaMap controlData approximate
            // for debug
            let path = Utils.myDebugFilePath ("fsa_" + methodName + ".dot")
            FsaHelper.toDot fsa path
            // end
            Some(fsa), restStack
        | _ -> failwith "wrong operation info type"