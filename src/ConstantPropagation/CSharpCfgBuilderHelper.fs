module CSharpCfgBuilderHelper

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.ControlFlow

let nodeToCSharpCfg (declaration : ITreeNode) = 
    let cSharpCfgBuilder = new CSharpControlFlowBuilder()
    let graph = cSharpCfgBuilder.GraphFromNode (declaration, null, false)
    graph :?> ICSharpControlFlowGraph