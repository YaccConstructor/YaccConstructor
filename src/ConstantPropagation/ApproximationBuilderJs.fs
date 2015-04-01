module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilderJs

open JetBrains.ReSharper.Psi.JavaScript.ControlFlow

open Utils

open System.IO

let serializeJsCfg (cfg: IJsControlFlowGraf) = 
    let name = "JsCfg_" + cfg.GetHashCode().ToString() + ".dot"
    let outFile = Path.Combine (myDebugFolderPath, name)
    DotUtils.cfgToDot cfg outFile "JsCfg"

let BuildApproximation (cfg: IJsControlFlowGraf) =
    serializeJsCfg cfg