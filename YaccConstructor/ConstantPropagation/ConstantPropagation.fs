namespace YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open JetBrains.ReSharper.Psi.CSharp;
open JetBrains.ReSharper.Psi.CSharp.Tree;
open JetBrains.ReSharper.Psi.Tree;
open JetBrains.ReSharper.Psi;
open JetBrains.ReSharper.Psi.Files;
open AbstractLexer.Common
open Microsoft.FSharp.Collections

type Approximator(file:ICSharpFile) = 
    let count = ref 0
    let propagate (hotspot:IInvocationExpression) =
        let args = hotspot.ArgumentList.Arguments |> Array.ofSeq
        //|> Seq.map (fun arg -> arg.)
        let start = args.[0].Value
        let edges = new ResizeArray<_>()
        let rec go (node:ITreeNode) =
            match node with
            | :? IAdditiveExpression as a ->
                go a.LeftOperand 
                go a.RightOperand
            | :? ICSharpLiteralExpression as l -> 
                new LexerEdge<_,_>(!count,(incr count; !count),Some(l.Literal.GetText().Trim[|'"'|],l))
                |> edges.Add
            | _ -> ()

        go start
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
