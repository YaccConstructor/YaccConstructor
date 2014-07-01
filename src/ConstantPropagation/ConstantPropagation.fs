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
        let x = graph.Inspect(ValueAnalysisMode.OPTIMISTIC)
        let defUses = x.AssignmentsUsage
        let count = ref 0
        let args = hotspot.ArgumentList.Arguments |> Array.ofSeq

        let edges = new ResizeArray<_>()

        let rec go start _end (node:ITreeNode) =
            let processVar start _end (v:IReferenceExpression) =
                let decl = v.Reference.CurrentResolveResult.DeclaredElement //.Resolve().DeclaredElement
                let assignments =
                    let x =  
                        defUses.[decl] 
                        |> Seq.filter (fun kvp -> kvp.Value <> null && kvp.Value.Contains v)
                    x
                    |> Seq.map (fun kvp -> 
                      match kvp.Key with
                      | :? ILocalVariableDeclaration as lvDecl -> (lvDecl.Initial :?> IExpressionInitializer).Value
                      | _ -> 
                        match kvp.Key.Parent with 
                        | :? IAssignmentExpression as ae -> (ae.Arguments.[1] :?> ExpressionArgumentInfo).Expression
                        | x -> failwithf "Unexpected parent type: %A" x
                      //| :? IMultipleLocalVariableDeclaration as vd -> vd.Declarators.[0] )
                      )
                    |> Array.ofSeq    
                assignments |> Array.iter (go start _end) 
            match node with
            | :? IAdditiveExpression as a ->
                let n = incr count; !count
                go start n a.LeftOperand 
                go n _end a.RightOperand
            | :? ICSharpLiteralExpression as l -> 
                new LexerEdge<_,_>(start,_end,Some(l.Literal.GetText().Replace("\\","").Trim[|'"'|],l))
                |> edges.Add
            | :? IReferenceExpression as re -> processVar start _end re
            | :? IConditionalTernaryExpression as tern -> 
                go start _end tern.ElseResult
                go start _end tern.ThenResult
            | x -> failwithf "Unexpected node type: %A" x

        go 0 (incr count; !count) args.[0].Value

        let res = new LexerInputGraph<_>()
        res.AddVerticesAndEdgeRange edges |> ignore
        res.StartVertex <- 0
        res

    member this.Approximate (defineLang: ITreeNode -> 'a) =
        let hotspots = new ResizeArray<_>() 
        let addHotspot (node:ITreeNode) =
            match node with 
            | :? IInvocationExpression as m 
                when Array.exists ((=) (m.InvocationExpressionReference.GetName().ToLowerInvariant())) [|"executeimmediate"; "eval"; "objnotation"|] 
                -> hotspots.Add (defineLang node , m)
            | _ -> ()
        //InvocationExpressionNavigator.
        let processor = RecursiveElementProcessor(fun x -> addHotspot x)
        processor.Process file
        let graphs = ResizeArray.map (fun (l,h) -> l, propagate h) hotspots
        graphs