namespace YS.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open JetBrains.ReSharper.Psi.CSharp;
open JetBrains.ReSharper.Psi.CSharp.Tree;
open JetBrains.ReSharper.Psi.Tree;
open JetBrains.ReSharper.Psi;
open JetBrains.ReSharper.Psi.Files;

type Approximator(file:ICSharpFile) = 
//    let propagate (hotspot:IFunction) =
//        match hotspot with
//        | 
    member this.Approximate () = 
        let isHotspot (node:ITreeNode) =
            match node with 
            | :? IMethod as m -> m.ShortName.ToLowerInvariant() = "executeimmediate"
            //| :? IIdentifier as i -> i.
            | _ -> false
        let hotspots = file.FindNodesAt(new TreeTextRange(file.GetTreeEndOffset()), fun x -> isHotspot x)
        ""
