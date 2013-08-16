namespace YS.ReSharper.LanguageApproximation.ConstantPropagation

open JetBrains.ReSharper.Psi.CSharp;
open JetBrains.ReSharper.Psi.CSharp.Tree;
open JetBrains.ReSharper.Psi.Tree;
open JetBrains.ReSharper.Psi;
open JetBrains.ReSharper.Psi.Files;


type Approximator(file:ICSharpFile) = 
    member this.Approximate () = 
        ""
