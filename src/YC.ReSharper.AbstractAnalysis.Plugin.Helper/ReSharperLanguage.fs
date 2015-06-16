module ReSharperLanguage

open YC.SDK.CommonInterfaces
open JetBrains.DocumentModel
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

type IReSharperLanguage = 
    inherit IInjectedLanguageModule<ICSharpLiteralExpression, DocumentRange, ITreeNode>