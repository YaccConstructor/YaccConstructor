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
open UserDefOperationInfo
open ReshrperCsharpTreeUtils

open System.Collections.Generic
open System.IO

let private tryDefineLang (node: IInvocationExpression) (hotspotInfoList: list<string * Hotspot>) = 
    let methodName, className, parameters, retType = getMethodSigniture node
    let retTypeName = retType.GetLongPresentableName(CSharpLanguage.Instance)
    hotspotInfoList
    |> List.tryFind 
        (
            fun hotspotInfo -> 
                let hotspot = snd hotspotInfo
                hotspot.Class = className.ToLowerInvariant()
                && hotspot.Method = methodName.ToLowerInvariant() 
                && parameters.[hotspot.QueryPosition].Type.IsString() 
                && hotspot.ReturnType = retTypeName.ToLowerInvariant()
        )
    |> Option.map fst

let findHotspots (file: ICSharpFile) (hotspotInfoList: list<string * Hotspot>) =
    let hotspots = new ResizeArray<_>() 
    let processNode (node: ITreeNode) =
        match node with 
        | :? IInvocationExpression as invocExpr  -> 
            tryDefineLang invocExpr hotspotInfoList
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()

    let processor = RecursiveElementProcessor(fun node -> processNode node)
    processor.Process file
    hotspots

let buildFsa (file: ICSharpFile) recursionMaxLevel =
    DotUtils.allMethodsCFGToDot file myDebugFolderPath

    let hotspotInfoList = XMLParser.parseXml "Hotspots.xml"
    let lang, hotspot = (findHotspots file hotspotInfoList).[0]
    let methodDeclaration = getEnclosingMethod hotspot

    let stringParamsNum = Seq.length <| getStringTypedParams methodDeclaration
    let stack = List.replicate stringParamsNum <| FsaHelper.anyWordsFsa ()
    let methodName = methodDeclaration.NameIdentifier.Name
    let hotVarRef = (hotspot.Arguments.[0].Value) :> ITreeNode
    let controlInfo = { 
        TargetMethod = methodName; 
        TargetNode = hotVarRef; 
        CurRecLevel = recursionMaxLevel }
    let fsaForVar = 
        approximate (CsharpArbitraryFun(methodDeclaration)) stack controlInfo
        |> fst
        |> Option.get
    fsaForVar


// this method is obsolete and will be deleted soon
let buildDdg (file: ICSharpFile) =
    // the next line is for debug purposes
    DotUtils.allMethodsCFGToDot file myDebugFolderPath

    let hotspotInfoList = XMLParser.parseXml "Hotspots.xml"
    let lang, hotspot = (findHotspots file hotspotInfoList).[0]
    let methodDeclaration = getEnclosingMethod hotspot
    let methodName = methodDeclaration.NameIdentifier.Name
    let csharpCFG = CSharpControlFlowBuilder.Build methodDeclaration
    let genericCFG, convertInfo = toGenericCfg csharpCFG methodName

    let genCfgName = "cfg_" + file.GetHashCode().ToString() 
    BidirectGraphFuns.toDot genericCFG.Graph genCfgName (myDebugFilePath (genCfgName + ".dot"))

    let hotVarRefCfe = (hotspot.Arguments.[0].Value) :> ITreeNode
    let hotVarRef = getMappingToOne hotVarRefCfe convertInfo.AstToGenericNodesMapping
    let ddg = GenericCFGFuncs.ddgForVar hotVarRef genericCFG

    let ddgName = "ddg_" + file.GetHashCode().ToString()
    BidirectGraphFuns.toDot ddg.Graph ddgName (myDebugFilePath (ddgName + ".dot"))

    ddg