module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximateCsharp

open QuickGraph

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.ControlFlow

open XMLParser
open Utils
open Utils.DictionaryFuns
open GenericCfgCsharp
open GenericGraphs
open FsaHelper
open GenerateFsa
open BuildApproximation

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
    CSharpControlFlowBuilder.Build methodDeclaration, methodDeclaration.NameIdentifier.Name

let buildCfg (file: ICSharpFile) =
    // the next line is for debug purposes
    DotUtils.allMethodsCFGToDot file myDebugFolderPath
    // process the first hotspot
    let lang, hotspot = (getHotspots file).[0]
    createControlFlowGraph hotspot

let buildDdg (file: ICSharpFile) =
    // the next line is for debug purposes
    DotUtils.allMethodsCFGToDot file myDebugFolderPath
    // process the first hotspot
    let lang, hotspot = (getHotspots file).[0]
    let csharpCFG, methodName = createControlFlowGraph hotspot
    let genericCFG, convertInfo = toGenericCfg csharpCFG methodName

    let genCfgName = "cfg_" + file.GetHashCode().ToString() 
    BidirectGraphFuns.toDot genericCFG.Graph genCfgName (myDebugFilePath (genCfgName + ".dot"))

    let hotVarRefCfe = (hotspot.Arguments.[0].Value) :> ITreeNode
    let hotVarRef = getMappingToOne hotVarRefCfe convertInfo.AstToGenericNodesMapping
    let ddg = GenericCFGFuncs.ddgForVar hotVarRef genericCFG

    let ddgName = "ddg_" + file.GetHashCode().ToString()
    BidirectGraphFuns.toDot ddg.Graph ddgName (myDebugFilePath (ddgName + ".dot"))

    ddg

let buildFsa (file: ICSharpFile) recursionMaxLevel =
    let ddg = buildDdg file
    let fsaForVar = buildAutomaton ddg Map.empty recursionMaxLevel fsaGenerator

    // for debug
    let fsaName = "fsa_" + file.GetHashCode().ToString()
    let path = Path.Combine (myDebugFolderPath, fsaName + ".dot")
    FsaHelper.toDot fsaForVar path

    fsaForVar