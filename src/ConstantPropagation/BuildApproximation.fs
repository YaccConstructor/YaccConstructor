module BuildApproximation

open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree

open UserDefOperationInfo
open CsharpCfgToGeneric
open GenericGraphElements
open GenericCFG
open DDG
open FsaHelper
open GenerateFsa
open Utils.DictionaryFuns
open ResharperCsharpTreeUtils

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

let rec approximate (functionInfo: ArbitraryOperation) (stack: list<CharFSA>) (controlData: ControlData) =
    if controlData.CurRecLevel < 0
    then None, stack
    else 
        match functionInfo.Info with
        | NoInfo -> None, stack
        | CsharpArbitraryFun (methodDecl) ->
            let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
            let methodName = methodDecl.NameIdentifier.Name
            let genericCFG, convertInfo = toGenericCfg csharpCfg methodName
            BidirectGraphFuns.toDot genericCFG.Graph methodName <| Utils.myDebugFilePath ("cfg_" + methodName + ".dot")
            let ddg =
                if methodName = controlData.TargetMethod
                then 
                    let targetNode = getMappingToOne controlData.TargetNode convertInfo.AstToGenericNodes
                    GenericCFGFuncs.ddgForVar targetNode genericCFG
                else GenericCFGFuncs.ddgForExits genericCFG
            BidirectGraphFuns.toDot ddg.Graph methodName <| Utils.myDebugFilePath ("preddg_" + methodName + ".dot")
            let ddg =
                if isTailRecursive methodName ddg
                then 
                    let stringParams = getStringTypedParams methodDecl |> List.ofSeq
                    tailRecursionToLoop methodName stringParams ddg
                else ddg
            BidirectGraphFuns.toDot ddg.Graph methodName <| Utils.myDebugFilePath ("ddg_" + methodName + ".dot")
            let initFsaMap, restStack = bindArgsToParams methodDecl stack
            let controlData = { controlData with CurRecLevel = controlData.CurRecLevel - 1 }
            let fsa = buildAutomaton ddg initFsaMap controlData approximate
            FsaHelper.toDot fsa <| Utils.myDebugFilePath ("fsa_" + methodName + ".dot")
            Some(fsa), restStack
        | _ -> failwith "wrong operation info type"