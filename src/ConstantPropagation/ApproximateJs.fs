module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximateJs

open JetBrains.ReSharper.Psi.JavaScript.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree

open Utils
open JsCfgToGeneric
open GenericGraphs
open Utils.DictionaryFuns
open BuildApproximation
open GenerateFsa

let serializeJsCfg (cfg: IJsControlFlowGraf) = 
    let name = "JsCfg_" + cfg.GetHashCode().ToString() + ".dot"
    DotUtils.cfgToDot cfg (myDebugFilePath name) "JsCfg"

let isHotspot (node: IInvocationExpression) = 
    let invoked = node.InvokedExpression :?> IReferenceExpression
    let name = invoked.Name
    name = "execScript"

let getHotspots (cfg: IJsControlFlowGraf) =
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

let build (jsCfg: IJsControlFlowGraf) =
    let fstHotspot = getHotspots jsCfg |> List.head :> ITreeNode
    let methodName = "main"
    let genericCFG, convertInfo = toGenericCfg jsCfg methodName
    // for debug
    let path = Utils.myDebugFilePath ("cfg_" + methodName + ".dot")
    BidirectGraphFuns.toDot genericCFG.Graph methodName path
    // end
    let ddg =
        let targetNode = getMappingToOne fstHotspot convertInfo.AstToGenericNodes
        GenericCFGFuncs.ddgForVar targetNode genericCFG
    // for debug
    let path = Utils.myDebugFilePath ("ddg_" + methodName + ".dot")
    BidirectGraphFuns.toDot ddg.Graph methodName path
    // end
    let initFsaMap = Map.empty
    let controlData = { TargetMethod = methodName; TargetNode = fstHotspot; CurRecLevel = 0 }
    let fsa = buildAutomaton ddg initFsaMap controlData approximate
    // for debug
    let path = Utils.myDebugFilePath ("fsa_" + methodName + ".dot")
    FsaHelper.toDot fsa path
    // end
    fsa

let BuildFsaForOneFunctionCfg (cfg: IJsControlFlowGraf) =
    serializeJsCfg cfg
    build cfg