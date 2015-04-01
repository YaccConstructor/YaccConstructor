module CFGConversionJs

open CSharpCFGInfo
open Utils

open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.JavaScript.Tree
open JetBrains.ReSharper.Psi.JavaScript.ControlFlow

let private extractJsCfgInfo (cfgElem: IControlFlowElement) (info: CSharpCFGInfo) =   
    let astCfgMap' = CfgUtils.addAstCfgMapping cfgElem info.AstCfgMap
    { info with AstCfgMap = astCfgMap' }
            
let collectAdditionalInfo (cfg: IJsControlFlowGraf) =
    CfgUtils.collectAdditionalInfo cfg extractJsCfgInfo emptyCSharpCfgInfo