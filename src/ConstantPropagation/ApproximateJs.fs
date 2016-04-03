/// Functions for building JavaScript's CFG approximation.
/// todo: Here approximation is built only for one Js CFG
/// because I don't know how to extract CFG from Js function calls,
/// so BuildApproximation module is not used here. Further investigation
/// of ReSharper's JavaScript support is nedded to be able to use
/// BuildApproximation module's functionality. Also hotspots info 
/// from Hotspots.xml is not used too because I don't know how to
/// extract needed info from Js method calls.
module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximateJs

open JetBrains.ReSharper.Psi.JavaScript.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree

open Utils
open JsCfgToGeneric
open GenericGraphs
open GenericCFG
open DDG
open Utils.Dictionary
open BuildApproximation
open GenerateFsa
open IControlFlowGraphUtils
open QuickGraph.FSA.GraphBasedFsa
open ArbitraryOperation

let private serializeJsCfg (cfg: IJsControlFlowGraph) = 
    let name = "JsCfg_" + cfg.GetHashCode().ToString() + ".dot"
    cfgToDot cfg (myDebugFilePath name) "JsCfg"

let private isHotspot (node: IInvocationExpression) = 
    let invoked = node.InvokedExpression :?> IReferenceExpression
    let name = invoked.Name
    name = "execScript"

let private getHotspots (cfg: IJsControlFlowGraph) =
    cfg.AllElements
    |> List.ofSeq
    |> List.choose
        (
            fun cfe -> 
                match cfe.SourceElement with
                | :? IInvocationExpression as invocExpr ->
                    if isHotspot invocExpr
                    then Some(invocExpr.Arguments.[0])
                    else None
                | _ -> None
        )

let rec approximateJs 
        (functionInfo: ArbitraryOperation<_>) 
        (stack: list<FSA<_>>) 
        (controlData: ControlData<_,_,_>) =
    None, stack

let private build (jsCfg: IJsControlFlowGraph) =
    let fstHotspot = getHotspots jsCfg |> List.head :> ITreeNode
    let methodName = "main"
    let genericCFG, convertInfo = toGenericCfg jsCfg methodName
    // for debug
    let path = Utils.myDebugFilePath ("cfg_" + methodName + ".dot")
    BidirectGraphFuns.toDot genericCFG.Graph methodName path
    // end
    let ddg =
        let targetNode = getMappingToOne fstHotspot convertInfo.AstToGenericNodes
        GenericCFGFuncs.ddgForNode targetNode genericCFG
    // for debug
    let path = Utils.myDebugFilePath ("ddg_" + methodName + ".dot")
    BidirectGraphFuns.toDot ddg.Graph methodName path
    // end
    let initFsaMap = Map.empty
    let controlData = { 
        TargetFunction = methodName; 
        TargetNode = fstHotspot; 
        CurRecLevel = 0;
        LoggerState = Logger.disabledLogger
        FsaParams = CharFsa.charFsaParams }
    let fsa = buildAutomaton ddg initFsaMap controlData approximateJs CharFsa.charFsaParams
    // for debug
    let path = Utils.myDebugFilePath ("fsa_" + methodName + ".dot")
    CharFsa.toDot fsa path
    // end
    fsa

/// Builds approximation for the first hotspot in a given Js function's CFG
let BuildFsaForOneFunctionCfg (cfg: IJsControlFlowGraph) =
    serializeJsCfg cfg
    build cfg