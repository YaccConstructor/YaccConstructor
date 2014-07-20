module AbstractAnalysis.Common




//let HandleErrors (ilp:IInjectedLanguageProcessor<_,_>) file = 
//    let parserErrors = new ResizeArray<_>()
//    let lexerErrors = new ResizeArray<_>()
//    let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
//        let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
//        brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
//        res.ToArray()
//
//    let defLang (n:ITreeNode) =
//        match n with 
//        | :? IInvocationExpression as m ->
//            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
//            | "executeimmediate" -> "TSQL" //injectedLanguages.["TSql"]
//            | "eval" -> "Calc" //injectedLanguages.["Calc"]
//            | "objnotation" -> "JSON" //injectedLanguages.["JSON"] 
//            | _ -> failwith "Unsupported language for AA!"
//        | _ -> failwith "Unexpected information type for language specification!"
//
//    let graphs = (new Approximator(file)).Approximate defLang //ToDo defLang to Core
//    let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
//        let ranges = 
//            brs |> Seq.groupBy (fun x -> x.back_ref)
//            |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
//            |> Seq.map(fun brs ->
//                try
//                    let pos =  brs |> Array.map(fun i -> i.pos_cnum)
//                    let lengthTok = pos.Length
//                    let beginPosTok = pos.[0] + 1
//                    let endPosTok = pos.[lengthTok-1] + 2 
//                    let endPos = 
//                        brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
//                        - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
//                    brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
//                with
//                | e -> 
//                    brs.[0].back_ref.GetDocumentRange())
//        ranges
//
//    let addError tok tokenToNumberFunc numToStringFunc (tokenDataFunc: _ -> obj)= 
//        let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
//            calculatePos brs 
//            |> Seq.iter
//                (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
//        let name = tok |> (tokenToNumberFunc >>  numToStringFunc)
//        let l,br = tokenDataFunc tok :?>_
//        e name l br
//
//    let error tok  = addError tok ilp.TokenToNumber ilp.NumToString ilp.TokenData 
//    
//    graphs
//    |> ResizeArray.iter 
//        (fun (l,g) -> processLang g ilp.Tokenize ilp.Parse lexerErrors.Add  (error l)) //(error token) //ToDo
//            (*match l with
//            | Calc -> processLang g l.tokenize l.parse lexerErrors.Add  error Calc*)
//
//    lexerErrors,parserErrors