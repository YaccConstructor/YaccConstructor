/// Main module for building string embedded languages regular approximation
module BuildApproximation

open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.Tree

open ArbitraryOperation
open CsharpCfgToGeneric
open GenericGraphs
open GenericCFG
open DDG
open FsaHelper
open GenerateFsa
open Utils.Dictionary
open ResharperCsharpTreeUtils
open CsharpApprxomationUtils
open YC.FSA.GraphBasedFsa

/// Contains control info for approximation algo. TargetFunction field
/// contains the name of a method or a function where hotspot node is located,
/// TargetNode field contains this node. 
/// CurRecLevel shows current allowed level of recursive calls for approximation algo.
/// If CurRecLevel >= 0 algorithm can process current function, and for every subsequent 
/// recursive call CurRecLevel will be decreased by 1.
/// LoggerState is used to log the approximation process
type ControlData<'a when 'a: equality> = {
    TargetFunction: string
    TargetNode: ITreeNode
    CurRecLevel: int
    LoggerState: Logger.LoggerState<'a> }

/// Builds approximation of a given function or method if ControlData.CurRecLevel >= 0
/// and "functionInfo" argument contains language specific info.
/// First of all language specific method's CFG is converted into generic CFG. Then
/// if current method is a target method DDG is extracted from generic CFG for a ControlData.TargetNode,
/// otherwise DDG is extracted for return statements of a method. Finally FSA is built for DDG
/// and returned as a result. FSA building algorithm may lead to recursive call of "approximate" function
/// if DDG contains other methods calls.
let rec approximateCSharp (functionInfo: ArbitraryOperation) (stack: list<FSA<_>>) 
                    (controlData: ControlData<_>) (fsaParams: FsaParams<_,_,_>) =
    if controlData.CurRecLevel < 0
    then None, stack
    else 
        match functionInfo.Info with
        | NoInfo -> None, stack
        | CsharpArbitraryFun (methodDecl) ->
            let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
            let methodName = methodDecl.NameIdentifier.Name
            let genericCFG, convertInfo = toGenericCfg csharpCfg methodName
            do Logger.logGenericCfg genericCFG.Graph methodName controlData.LoggerState
            let ddg =
                if methodName = controlData.TargetFunction
                then 
                    let targetNode = getMappingToOne controlData.TargetNode convertInfo.AstToGenericNodes
                    GenericCFGFuncs.ddgForNode targetNode genericCFG
                else GenericCFGFuncs.ddgForExits genericCFG
            do Logger.logPreDdg ddg.Graph methodName controlData.LoggerState
            let ddg =
                if isTailRecursive methodName ddg
                then 
                    let stringParams = getStringTypedParams methodDecl |> List.ofSeq
                    tailRecursionToLoop methodName stringParams ddg
                else ddg
            do Logger.logDdg ddg.Graph methodName controlData.LoggerState
            let initFsaMap, restStack = bindArgsToParams methodDecl stack
            let controlData = { controlData with CurRecLevel = controlData.CurRecLevel - 1 }
            let fsa = buildAutomaton ddg initFsaMap controlData approximateCSharp fsaParams
            do Logger.logFsa fsa methodName controlData.LoggerState
            Some(fsa), restStack
        | _ -> failwith "wrong operation info type"

let rec approximateJs (functionInfo: ArbitraryOperation) (stack: list<FSA<_>>) 
                    (controlData: ControlData<_>) (fsaParams: FsaParams<_,_,_>) =
    None, stack