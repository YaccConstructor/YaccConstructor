/// Main module for building string embedded languages regular approximation
module BuildApproximation

open ArbitraryOperation
open GenericGraphs
open GenericCFG
open DDG
open GenerateFsa
open QuickGraph.FSA.GraphBasedFsa

/// Contains control info for approximation algo. TargetFunction field
/// contains the name of a method or a function where hotspot node is located,
/// TargetNode field contains this node. 
/// CurRecLevel shows current allowed level of recursive calls for approximation algo.
/// If CurRecLevel >= 0 algorithm can process current function, and for every subsequent 
/// recursive call CurRecLevel will be decreased by 1.
/// LoggerState is used to log the approximation process
type ControlData<'n, 'a, 'b when 'a: equality and 'b: equality> = {
    TargetFunction: string
    TargetNode: 'n
    CurRecLevel: int
    LoggerState: Logger.LoggerState<'b>
    FsaParams: FsaParams<'a,'b> }

/// Builds approximation of a given function or method if ControlData.CurRecLevel >= 0
/// and "functionInfo" argument contains language specific info.
/// First of all language specific method's CFG is converted into generic CFG. Then
/// if current method is a target method DDG is extracted from generic CFG for a ControlData.TargetNode,
/// otherwise DDG is extracted for return statements of a method. Finally FSA is built for DDG
/// and returned as a result. FSA building algorithm may lead to recursive call of "approximate" function
/// if DDG contains other methods calls.
let rec approximate
        (extractLangSpecificCfg: ArbitraryOperationInfo<'OpInfo> -> 'SpecificCfg)
        (toGenericCfg: 'SpecificCfg -> string -> GenericCFG<_,_> * 'ConvertInfo)
        (extractTargetNode: 'Node -> 'ConvertInfo -> GraphNode<_,_>)
        (getStringTypedParams: 'OpInfo -> seq<string>)
        (bindArgsToParams: 'OpInfo -> list<FSA<_>> -> Map<string, FSA<_>> * list<FSA<_>>)
        (operation: ArbitraryOperation<'OpInfo>) 
        (stack: list<FSA<_>>) 
        (controlData: ControlData<'Node, _, _>) =
    if controlData.CurRecLevel < 0
    then None, stack
    else 
        match operation with
        | None -> None, stack
        | Some (operationInfo) ->
            let languageSpecificCfg = extractLangSpecificCfg operationInfo
            let genericCFG, convertInfo = toGenericCfg languageSpecificCfg operationInfo.Name
            do Logger.logGenericCfg genericCFG.Graph operationInfo.Name controlData.LoggerState
            let ddg =
                if operationInfo.Name = controlData.TargetFunction
                then 
                    let targetNode = extractTargetNode controlData.TargetNode convertInfo
                    GenericCFGFuncs.ddgForNode targetNode genericCFG
                else GenericCFGFuncs.ddgForExits genericCFG
            do Logger.logPreDdg ddg.Graph operationInfo.Name controlData.LoggerState
            let ddg =
                if isTailRecursive operationInfo.Name ddg
                then 
                    let stringParams = getStringTypedParams operationInfo.Info |> List.ofSeq
                    tailRecursionToLoop operationInfo.Name stringParams ddg
                else ddg
            do Logger.logDdg ddg.Graph operationInfo.Name controlData.LoggerState
            let initFsaMap, restStack = bindArgsToParams operationInfo.Info stack
            let controlData = { controlData with CurRecLevel = controlData.CurRecLevel - 1 }
            let approimateForRecCall = 
                approximate 
                    extractLangSpecificCfg toGenericCfg extractTargetNode getStringTypedParams bindArgsToParams
            let fsa = buildAutomaton ddg initFsaMap controlData approimateForRecCall controlData.FsaParams
            do Logger.logFsa fsa operationInfo.Name controlData.LoggerState
            Some(fsa), restStack