namespace YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open AbstractLexer.Common
open Microsoft.FSharp.Collections
open JetBrains.ReSharper.Psi.ControlFlow
open JetBrains.ReSharper.Psi.ControlFlow.CSharp
open JetBrains.ReSharper.Psi.CSharp.Impl.Resolve

type Approximator(file:ICSharpFile) = 
    let propagate (hotspot:IInvocationExpression) =
        let declaration = hotspot.FindPrevNode(fun node -> match node with :? ICSharpFunctionDeclaration -> TreeNodeActionType.ACCEPT |_ ->  TreeNodeActionType.CONTINUE)
        let graph = CSharpControlFlowBuilder.Build(declaration :?> ICSharpFunctionDeclaration)
        let x = graph.Inspect(ValueAnalysisMode.PESSIMISTIC)//  AllElements
        let c = x.AssignmentsUsage //GetVariableStateAt(graph.BodyElement,declaration :?> IDeclaredElement)        
        let count = ref 0
        let args = hotspot.ArgumentList.Arguments |> Array.ofSeq
        //|> Seq.map (fun arg -> arg.)
        let vars = args |> Array.filter(fun v -> v.Value :? IReferenceExpression) |> Array.map (fun v -> v.Value :?> IReferenceExpression)
        let decls = vars |> Array.map (fun v -> v.Reference.CurrentResolveResult.DeclaredElement)
        //Resolve().DeclaredElement

        let edges = new ResizeArray<_>()

        let rec go start _end (node:ITreeNode) =
            let processVar start _end (v:IReferenceExpression) =
                let decl = v.Reference.Resolve().DeclaredElement //CurrentResolveResult.DeclaredElement
                let assignments = 
                //MultipleLocalVariableDeclaration
                    c.[decl] 
                    |> Seq.filter (fun kvp -> kvp.Value <> null && kvp.Value.Contains v) |> Seq.map (fun kvp -> 
                      match kvp.Key.Parent with
                      | :? IAssignmentExpression as ae -> (ae.Arguments.[1] :?> ExpressionArgumentInfo).Expression
                      //| :? IMultipleLocalVariableDeclaration as vd -> vd.Declarators.[0] )
                      )
                    |> Array.ofSeq    
//                let start = !count
//                let _end = incr count; !count
                assignments |> Array.iter (go start _end) 
            match node with
            | :? IAdditiveExpression as a ->
                let n = incr count; !count
                go start n a.LeftOperand 
                go n _end a.RightOperand
            | :? ICSharpLiteralExpression as l -> 
                new LexerEdge<_,_>(start,_end,Some(l.Literal.GetText().Trim[|'"'|],l))
                |> edges.Add
            | :? IReferenceExpression as re -> processVar start _end re
            | _ -> ()

        
        //+		[JetBrains.ReSharper.Psi.CSharp.Impl.Resolve.ExpressionArgumentInfo]	{JetBrains.ReSharper.Psi.CSharp.Impl.Resolve.ExpressionArgumentInfo}	JetBrains.ReSharper.Psi.CSharp.Impl.Resolve.ExpressionArgumentInfo

        let start = args.[0].Value
        
        

        go 0 (incr count; !count) start

        let res = new LexerInputGraph<_>()
        res.AddVerticesAndEdgeRange edges |> ignore
        res.StartVertex <- 0
        res

        //ICshLiteralEx
        //referenceExpression
    member this.Approximate () =
        let hotspots = new ResizeArray<_>() 
        let addHotspot (node:ITreeNode) =
            match node with 
            | :? IInvocationExpression as m when m.InvocationExpressionReference.GetName().ToLowerInvariant() = "executeimmediate" -> hotspots.Add m
            | _ -> ()
        //InvocationExpressionNavigator.
        let processor = RecursiveElementProcessor(fun x -> addHotspot x)
        processor.Process file
        let graphs = ResizeArray.map propagate hotspots
        graphs


//        let hs4 = 
//            let processor = RecursiveElementProcessor(fun x -> addHotspot x)
//            processor.Process file
//        //let hs3 = file.
//        let hs2 = file.Children()// FindNodesAt(new TreeTextRange(file.GetTreeEndOffset()), fun x -> isHotspot x)
//        (hotspots |> Seq.map string |> String.concat "; ")
//        + (hs2 |> Seq.map string |> String.concat "; ")
//        + (hs3 |> Seq.map string |> String.concat "; ")
