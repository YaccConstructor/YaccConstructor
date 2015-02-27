module YC.ReSharper.AbstractAnalysis.LanguageApproximation.Approximator

open JetBrains.ReSharper.Psi.CSharp.Tree

let BuildApproximation (file: ICSharpFile) = 
    ApproximationBuilderCSharp.build file