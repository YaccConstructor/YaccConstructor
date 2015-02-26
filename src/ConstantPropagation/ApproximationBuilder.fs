module YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximationBuilder

open XMLParser
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.ControlFlow.CSharp

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

let private getDataFlowGraphExceptionMsg = "can't get enclosing method, null parent encountered"

let private getDataFlowGraph (hotspot: IInvocationExpression) =
    let rec getEnclosingMethod (node: ITreeNode) =
        match node with
        | null -> failwith getDataFlowGraphExceptionMsg
        | :? ICSharpFunctionDeclaration as funDecl -> funDecl
        | _ -> getEnclosingMethod node.Parent
    let methodDeclaration = getEnclosingMethod hotspot
    let controlFlowGraph = CSharpControlFlowBuilder.Build methodDeclaration
    ()

let Build (file: ICSharpFile) = 
    let hotspots = new ResizeArray<_>() 
    let addHotspot (node: ITreeNode) =
        match node with 
        | :? IInvocationExpression as invocExpr  -> 
            tryDefineLang invocExpr
            |> Option.iter (fun lang -> hotspots.Add (lang, invocExpr))
        | _ -> ()
    let processor = RecursiveElementProcessor(fun x -> addHotspot x)
    processor.Process file
    hotspots |> List.ofSeq |> List.iter (getDataFlowGraph << snd)