module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilderCSharp

open QuickGraph

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow

open XMLParser
open CSharpCFGInfo
open Utils
open CFGConversionCSharp
open GenericCFG
open GenericCFG.GenericCFGFuncs
open ForwardAnalysis

open System.Collections.Generic
open System.IO

let private hotspotInfoList = parseXml "Hotspots.xml"

let private tryDefineLang (node: IInvocationExpression) = 
    let typeDecl = node.InvokedExpression.GetText().Split('.')
    let className = typeDecl.[0].ToLowerInvariant()
    let methodName = typeDecl.[1].ToLowerInvariant()
                
    let args = node.AllArguments false
    let argTypes = new ResizeArray<_>()
    for argument in args do
        argTypes.Add <| argument.GetExpressionType().GetLongPresentableName(CSharpLanguage.Instance).ToLowerInvariant()
        
    let retType = node.GetExpressionType().GetLongPresentableName(CSharpLanguage.Instance).ToLowerInvariant()

    hotspotInfoList
    |> List.tryFind 
        (
            fun hotspotInfo -> 
                let hotspot = snd hotspotInfo
                hotspot.Class = className 
                && hotspot.Method = methodName 
                && argTypes.[hotspot.QueryPosition] = "string" 
                && hotspot.ReturnType = retType
        )
    |> Option.map fst  

let getHotspots (file: ICSharpFile) =
    let hotspots = new ResizeArray<_>() 
    let addHotspot (node: ITreeNode) =
        match node with 
        | :? IInvocationExpression as invocExpr  -> 
            tryDefineLang invocExpr
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()

    let processor = RecursiveElementProcessor(fun x -> addHotspot x)
    processor.Process file
    hotspots

let private getEnclosingMethodNullParentMsg = "can't get enclosing method, null parent encountered"

let private createControlFlowGraph (hotspot: IInvocationExpression) =
    let rec getEnclosingMethod (node: ITreeNode) =
        match node with
        | null -> failwith getEnclosingMethodNullParentMsg
        | :? ICSharpFunctionDeclaration as funDecl -> funDecl
        | _ -> getEnclosingMethod node.Parent

    let methodDeclaration = getEnclosingMethod hotspot
    CSharpControlFlowBuilder.Build methodDeclaration

let build (file: ICSharpFile) =
    let hotspots = getHotspots file
    let ddGraphs = 
        hotspots 
        |> List.ofSeq 
        |> List.map (fun (lang, hotspot) -> createControlFlowGraph hotspot, hotspot)
        |> List.map
            (
                fun (csharpCFG, hotspot) ->
                    let additionalInfo = collectAdditionalInfo csharpCFG
                    let genericCFG = convert csharpCFG additionalInfo
                    let hotVarRef = extractVarRefFromHotspot hotspot additionalInfo genericCFG
                    let cfgForVar = subgraphForVar hotVarRef genericCFG
                    let fsaForVar = buildAutomaton cfgForVar

                    let cfgName = "cfg_" + csharpCFG.GetHashCode().ToString()
                    let path = Path.Combine (myDebugFolderPath, cfgName + ".dot")
                    toDot cfgForVar cfgName path

                    let fsaName = "fsa_" + csharpCFG.GetHashCode().ToString()
                    let path = Path.Combine (myDebugFolderPath, fsaName + ".dot")
                    FsaHelper.toDot fsaForVar path

                    cfgForVar
            )
    ddGraphs