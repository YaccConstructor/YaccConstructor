/// Functions for working with ReSharper's representation of C# source files
module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximateCsharp

open QuickGraph

open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.ControlFlow
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow

open Hotspot
open Utils
open ArbitraryOperation
open ResharperCsharpTreeUtils
open BuildApproximation
open ResharperCsharpTreeUtils
open Microsoft.FSharp.Collections
open QuickGraph.FSA.GraphBasedFsa
open ResharperCfgToGeneric
open QuickGraph.FSA.FsaApproximation

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

let private findHotspots (file: ICSharpFile) (hotspotInfoList: list<string * Hotspot>) =
    let hotspots = new ResizeArray<_>() 
    let processNode (node: ITreeNode) =
        match node with 
        | :? IInvocationExpression as invocExpr ->
            tryDefineLang invocExpr hotspotInfoList
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()

    let processor = RecursiveElementProcessor(fun node -> processNode node)
    processor.Process file
    hotspots

let rec approximateCSharp 
        (functionInfo: ArbitraryOperation<IMethodDeclaration>) 
        (stack: list<FSA<_>>) 
        (controlData: ControlData<ITreeNode, char, char * Position<_>>) =
    let extractLangSpecificCfg (methodInfo: ArbitraryOperationInfo<IMethodDeclaration>) = 
        nodeToCSharpCfg methodInfo.Info
    let extractTargetNode (astNode: ITreeNode) (convertInfo: ConvertInfo<_,_>) =
        Utils.Dictionary.getMappingToOne astNode convertInfo.AstToGenericNodes
    approximate 
        extractLangSpecificCfg 
        CsharpCfgToGeneric.toGenericCfg 
        extractTargetNode
        ResharperCsharpTreeUtils.getStringTypedParams
        CsharpApprxomationUtils.bindArgsToParams
        functionInfo
        stack
        controlData

let private buildFsaForMethod methodDecl target recursionMaxLevel fsaParams logger =
    let stringParamsNum = Seq.length <| getStringTypedParams methodDecl
    let stack = List.replicate stringParamsNum <| FSA<_>.CreateAnyWordsFsa fsaParams
    let methodName = methodDecl.NameIdentifier.Name
    let controlInfo = { 
        TargetFunction = methodName; 
        TargetNode = target; 
        CurRecLevel = recursionMaxLevel
        LoggerState = logger;
        FsaParams = fsaParams }
    let fsaForVar = 
        let functionInfo = Some { Name = methodName; Info = methodDecl }
        approximateCSharp functionInfo stack controlInfo
        |> fst
        |> Option.get
    fsaForVar

/// Finds hotspots in the given file and builds approximation
/// for them, starting only from enclosing method.
/// todo: approximation can be built not only for enclosing method
let ApproximateFileWithParams (file: ICSharpFile) recursionMaxLevel hotspotInfoList fsaParams logger =
    //let hotspotInfoList = HotspotParser.parseHotspots "..\\..\\..\\..\\ConstantPropagation\\Hotspots.xml"
    let hotspots = findHotspots file hotspotInfoList
    hotspots
    |> ResizeArray.map 
        (
            fun (lang, invocExpr) -> 
                let methodDeclaration = getEnclosingMethod invocExpr
                let hotVarRef = (invocExpr.Arguments.[0].Value) :> ITreeNode
                let fsa = buildFsaForMethod methodDeclaration hotVarRef recursionMaxLevel fsaParams logger
                lang, fsa
        )

/// Finds hotspots in the given file and builds approximation
/// for them, starting only from enclosing method. Logs approximation process
let ApproximateFileWithLogging (file: ICSharpFile) recursionMaxLevel hotspotInfoList =
    let loggerSt = Logger.create Utils.myDebugFilePath true CharFsa.toDot
    ApproximateFileWithParams file recursionMaxLevel hotspotInfoList CharFsa.charFsaParams loggerSt

/// Finds hotspots in the given file and builds approximation
/// for them, starting only from enclosing method. Logging is disabled
let ApproximateFile (file: ICSharpFile) recursionMaxLevel hotspotInfoList =
    // allMethodsCfgToDot file myDebugFolderPath
    // let loggerSt = Logger.create Utils.myDebugFilePath true FsaHelper.toDot
    ApproximateFileWithParams file recursionMaxLevel hotspotInfoList CharFsa.charFsaParams Logger.disabledLogger

// stub
let private buildInvocationTree (node: IInvocationExpression) =
    let services = node.GetContainingFile().GetPsiServices()
    let methDecl = node.InvocationExpressionReference.Resolve().DeclaredElement

    let refs = Search.FinderExtensions.FindAllReferences(services.Finder, methDecl)
    let nodes = refs |> Seq.map (fun r -> r.GetTreeNode())
    let enclMethods = nodes |> Seq.map (fun n -> getEnclosingMethod n)
    ()