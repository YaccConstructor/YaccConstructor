module YC.ReSharper.AbstractAnalysis.LanguageApproximation.Approximator

open JetBrains.ReSharper.Psi.CSharp.Tree

open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Utils

let BuildApproximation (file: ICSharpFile) = 
    CFGUtils.methodsCFGToDot file
    ApproximationBuilderCSharp.build file