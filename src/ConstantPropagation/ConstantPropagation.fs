namespace YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximateCsharp

open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi

open JetBrains.ReSharper.Psi.Files
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.ControlFlow.Impl
open JetBrains.ReSharper.Psi.CSharp.ControlFlow
open JetBrains.ReSharper.Psi.CSharp.Impl.Resolve


open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp.ControlFlow

open System.IO

open IControlFlowGraphUtils

type Approximator(file:IFile) = 
    // 0 for top and 3 level down
    let recursionMaxLevel = 3

    member this.Approximate() = 
        
        let res = new ResizeArray<_>()
        let lang, fsa = 
            match file with 
            | :? ICSharpFile as csFile -> 
                ApproximateFile csFile recursionMaxLevel
            | _ -> failwithf "Sorry, this file type doesn't supported now"

        
        res.Add((lang, fsa))
        res
            

    