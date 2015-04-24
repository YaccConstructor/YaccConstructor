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
open GenericGraphs
open GenericGraphs.GenericCFGFuncs
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

let buildDdg (file: ICSharpFile) =
    // the next line is for debug purposes
    DotUtils.allMethodsCFGToDot file myDebugFolderPath
    // process the first hotspot
    let lang, hotspot = (getHotspots file).[0]
    let csharpCFG = createControlFlowGraph hotspot
    let additionalInfo = collectAdditionalInfo csharpCFG
    let genericCFG = convert csharpCFG additionalInfo
    let hotVarRef = extractVarRefFromHotspot hotspot additionalInfo genericCFG
    let ddg = ddgForVar hotVarRef genericCFG

    // for debug
    let ddgName = "ddg_" + file.GetHashCode().ToString()
    let path = Path.Combine (myDebugFolderPath, ddgName + ".dot")
    DDGFuncs.toDot ddg ddgName path

    ddg

let buildFsa (file: ICSharpFile) =
    let ddg = buildDdg file
    let fsaForVar = buildAutomaton ddg Map.empty

    // for debug
    let fsaName = "fsa_" + file.GetHashCode().ToString()
    let path = Path.Combine (myDebugFolderPath, fsaName + ".dot")
    FsaHelper.toDot fsaForVar path

    fsaForVar

let BuildApproximation (file: ICSharpFile) = 
    // the next line is for debug purposes
    DotUtils.allMethodsCFGToDot file myDebugFolderPath
    buildFsa file



//let buildApproxForTarget (methodDecl: ICSharpFunctionDeclaration) (targetExpr: ICSharpExpression) initialFsaMap =
//    let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
//    let additionalInfo = collectAdditionalInfo csharpCfg
//    let genericCfg = convert csharpCfg additionalInfo
//    let targetExprCfe = additionalInfo.AstCfgMap.[hash targetExpr] |> List.ofSeq |> List.head
//    let ddg = ddgForNode targetExprCfe.Value.Id genericCfg
//    buildAutomaton ddg initialFsaMap
//
//let buildApproxForWholeMethod (methodDecl: ICSharpFunctionDeclaration) initialFsaMap =
//    let csharpCfg = CSharpControlFlowBuilder.Build methodDecl
//    let additionalInfo = collectAdditionalInfo csharpCfg
//    let genericCfg = convert csharpCfg additionalInfo
//    let exitNode = genericCfg.Vertices |> Seq.find (fun v -> genericCfg.OutDegree(v) = 0)
//    let ddg = ddgForNode exitNode.Id genericCfg
//    buildAutomaton ddg initialFsaMap

//let BuildApproximation (file: ICSharpFile) =
//    let collector = RecursiveElementCollector(fun (node: ITreeNode) -> node :? IMethodDeclaration)
//    let methods = collector.GetResults() |> Seq.map (fun n -> n :?> IMethodDeclaration)
//    methods 
//    |> Seq.filter (fun m -> Seq.isEmpty m.DeclaredElement.Parameters)
//    |> Seq.map (fun m -> buildApproxForWholeMethod m Map.empty)
//    |> Seq.iter (fun fsa -> FsaHelper.toDebugDot )