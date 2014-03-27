namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
open JetBrains.ReSharper.Daemon.CSharp.Stages
open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Feature.Services.CSharp.Bulbs
open JetBrains.ReSharper.Feature.Services.LinqTools
open JetBrains.ReSharper.Intentions.Extensibility
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages
open Yard.Examples.MSParser

type SupportedLangs =
    | Calc
    | TSQL
    | JSON

type Processor(file) =
    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | "objnotation" ->JSON
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let processLang graph tokenize parse addLError addPError = 
        let tokenize g =
            try 
               tokenize g
               |> Some 
            with
            | Calc.Lexer.LexerError(t,brs) ->
                (t, (brs :?> array<AbstractLexer.Core.Position<ICSharpLiteralExpression>>).[0].back_ref.GetDocumentRange())
                |> addLError
                None
        tokenize graph |> Option.map parse
        |> Option.iter
            (function 
             | Yard.Generators.RNGLR.Parser.Success(_,_) -> ()
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )
            
//(provider: ICSharpContextActionDataProvider) = 
    member this.Process () = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
            brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
            res.ToArray()
        //let sourceFile = provider.SourceFile
        //let file = provider.SourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) :?> ICSharpFile
        let graphs = (new Approximator(file)).Approximate defLang
        let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            try
                let pos = brs |> Array.map(fun i -> i.pos_cnum)
                let lengthTok = pos.Length
                let beginPosTok = pos.[0] + 1
                let endPosTok = pos.[lengthTok-1] + 2 
                let endPos = 
                    brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
                    - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
                brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
            with
            | :? System.ArgumentOutOfRangeException -> brs.[0].back_ref.GetDocumentRange()

        let addError tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                let newDr = calculatePos brs
                brs |> filterBrs 
                |> Array.iter
            let name = tok |> (Calc.AbstractParser.tokenToNumber >>  Calc.AbstractParser.numToString)
            let l,br = Calc.AbstractParser.tokenData tok :?>_
            e name l br
                    
        
        let addErrorJSON tok = 
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            let name = tok |> (JSON.Parser.tokenToNumber >> JSON.Parser.numToString)
            let l, br = JSON.Parser.tokenData tok :?>_
            e name l br
            
                    
       (* let addErrorTSQL tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            let name = tok |> (Yard.Examples.MSParser.tokenToNumber >> Yard.Examples.MSParser.numToString)
            let l, br = Yard.Examples.MSParser.tokenData tok :?>_
            e name l br *)


        graphs
        |> ResizeArray.iter 
            (fun (l,g) ->
                match l with
                | Calc -> processLang g Calc.tokenize Calc.parse lexerErrors.Add  addError
               (* | TSQL -> processLang g TSQL.tokenize TSQL.parse lexerErrors.Add  addErrorTSQL *)
                | JSON -> processLang g JSON.tokenize JSON.parse lexerErrors.Add  addErrorJSON )

        lexerErrors,parserErrors