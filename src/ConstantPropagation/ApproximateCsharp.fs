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
open UserDefOperationInfo
open ResharperCsharpTreeUtils
open BuildApproximation

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
        | :? IInvocationExpression as invocExpr ->
            tryDefineLang invocExpr hotspotInfoList
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()

    let processor = RecursiveElementProcessor(fun node -> processNode node)
    processor.Process file
    hotspots

let buildFsaForMethod methodDecl target recursionMaxLevel =
    let stringParamsNum = Seq.length <| getStringTypedParams methodDecl
    let stack = List.replicate stringParamsNum <| FsaHelper.anyWordsFsa ()
    let methodName = methodDecl.NameIdentifier.Name
    let controlInfo = { 
        TargetMethod = methodName; 
        TargetNode = target; 
        CurRecLevel = recursionMaxLevel }
    let fsaForVar = 
        let functionInfo = { Name = methodName; Info = CsharpArbitraryFun(methodDecl) }
        approximate functionInfo stack controlInfo
        |> fst
        |> Option.get
    fsaForVar

let ApproximateFile (file: ICSharpFile) recursionMaxLevel =
    // debug
    DotUtils.allMethodsCFGToDot file myDebugFolderPath
    // end
    let hotspotInfoList = XMLParser.parseXml "Hotspots.xml"
    // only the first hotspot is processed in currect implementation
    let lang, hotspot = (findHotspots file hotspotInfoList).[0]
    let methodDeclaration = getEnclosingMethod hotspot
    let hotVarRef = (hotspot.Arguments.[0].Value) :> ITreeNode
    buildFsaForMethod methodDeclaration hotVarRef recursionMaxLevel

// stub
let buildInvocationTree (node: IInvocationExpression) =
    let services = node.GetContainingFile().GetPsiServices()
    let methDecl = node.InvocationExpressionReference.Resolve().DeclaredElement

    let refs = Search.FinderExtensions.FindAllReferences(services.Finder, methDecl)
    let nodes = refs |> Seq.map (fun r -> r.GetTreeNode())
    let enclMethods = nodes |> Seq.map (fun n -> getEnclosingMethod n)
    ()