module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilderJs

open JetBrains.ReSharper.Psi.JavaScript.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree

open Utils
open CFGConversionJs
open GenericCFG.GenericCFGFuncs

open System.IO

let serializeJsCfg (cfg: IJsControlFlowGraf) = 
    let name = "JsCfg_" + cfg.GetHashCode().ToString() + ".dot"
    let outFile = Path.Combine (myDebugFolderPath, name)
    DotUtils.cfgToDot cfg outFile "JsCfg"

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

let build (cfg: IJsControlFlowGraf) =
    let fstHotspot = getHotspots cfg |> List.head

    let additionalInfo = collectAdditionalInfo cfg
    let genericCFG = convert cfg additionalInfo
    let hotVarRef = extractVarRefFromHotspot fstHotspot additionalInfo genericCFG
    let cfgForVar = subgraphForVar hotVarRef genericCFG

    let cfgName = "cfg_" + cfg.GetHashCode().ToString() + "_forvar"
    let path = Path.Combine (myDebugFolderPath, cfgName + ".dot")
    toDot cfgForVar cfgName path

    cfgForVar

let BuildApproximation (cfg: IJsControlFlowGraf) =
    serializeJsCfg cfg
    build cfg