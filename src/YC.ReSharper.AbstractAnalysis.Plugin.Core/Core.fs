﻿namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
//open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages
open Yard.Examples.MSParser
open AbstractAnalysis.Common

type SupportedLangs =
    | Calc
    | TSQL
    | JSON

type Processor(file) =
    let mutable calcXmlPath = ""
    let mutable jsonXmlPath = ""
    let mutable tsqlXmlPath = ""

    let mutable currentLang = Calc
    
    let mutable calcForest = []
    let mutable jsonForest = []
    let mutable tsqlForest = []

    ///Needs for tree generation for highlighting
    let mutable unprocessed = []
    let mutable count = 0

    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | "objnotation" ->JSON
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let processLang graph tokenize parse addLError addPError translate printer addSPPF = 
        let tokenize g =
            try 
               tokenize g
               |> Some 
            with
            | LexerError(t,brs) ->
                (t, (brs :?> array<AbstractLexer.Core.Position<ICSharpLiteralExpression>>).[0].back_ref.GetDocumentRange())
                |> addLError
                None

        tokenize graph |> Option.map parse
        |> Option.iter
            (function 
             | Yard.Generators.RNGLR.Parser.Success (sppf, _,errors) -> addSPPF (sppf, errors)
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )
         
    let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
        let ranges = 
            brs |> Seq.groupBy (fun x -> x.back_ref)
            |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
            |> Seq.map(fun brs ->
                try
                    let pos =  brs |> Array.map(fun i -> i.pos_cnum)
                    let lengthTok = pos.Length
                    let beginPosTok = pos.[0] + 1
                    let endPosTok = pos.[lengthTok-1] + 2 
                    let endPos = 
                        brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
                        - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
                    brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
                with
                | e -> 
                    brs.[0].back_ref.GetDocumentRange())
        ranges    


    let getNextTree (forest : list<Yard.Generators.RNGLR.AST.Tree<'TokenType> * _>) translate = 
        if forest.Length <= count 
        then
            count <- 0 
            null
        else 
            let mutable curSppf, errors = List.nth forest count
            if unprocessed.Length = 0
            then 
                unprocessed <- Array.init curSppf.TokensCount (fun i -> i) |> List.ofArray
            
            let nextTree, unproc = curSppf.GetNextTree unprocessed (fun _ -> true)
            if unproc.Length = 0
            then 
                count <- count + 1
            unprocessed  <- unproc

            (Seq.head <| translate nextTree errors) :> ITreeNode

    let getForestWithToken (forest : list<Yard.Generators.RNGLR.AST.Tree<'TokenType> * _>) 
                            range (tokenData: 'TokenType -> obj)  translate = 
        let tokenToPos (token : 'TokenType) = 
            let data = unbox <| tokenData token
            let str : string = fst data
            let pos : array<AbstractLexer.Core.Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> 
                = snd data
                
            calculatePos pos
        
        let res = new ResizeArray<_>()
        for ast, errors in forest do
            let trees = ast.GetForestWithToken range tokenToPos
            for tree in trees do
                let treeNode = Seq.head <| translate tree errors :> ITreeNode
                res.Add treeNode
        res

    member this.Graphs () =  (new Approximator(file)).Approximate defLang
    
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
        let addError tok tokenToNumberFunc numToStringFunc (tokenDataFunc: _ -> obj) =
            let e t l (brs:array<AbstractLexer.Core.Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>) = 
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            let name = tok |> (tokenToNumberFunc >>  numToStringFunc)
            let (l:string),br = tokenDataFunc tok :?> _
            e name l br

        (*
        let addErrorCalc tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
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
                    
        let addErrorTSQL tok =
            let e t l (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) = 
                calculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            let name = tok |> (Yard.Examples.MSParser.tokenToNumber >> Yard.Examples.MSParser.numToString)
            let l, br = Yard.Examples.MSParser.tokenData tok :?>_
            e name l br
                    
*)
        let errorCalc tok  = addError tok Calc.AbstractParser.tokenToNumber Calc.AbstractParser.numToString Calc.AbstractParser.tokenData 
        let errorJSON tok  = addError tok JSON.Parser.tokenToNumber JSON.Parser.numToString JSON.Parser.tokenData
        let errorTSQL tok  = addError tok Yard.Examples.MSParser.tokenToNumber Yard.Examples.MSParser.numToString Yard.Examples.MSParser.tokenData 

        let addCalcSPPF pair = calcForest <- calcForest @ [pair]
        let addJsonSPPF pair = jsonForest <- jsonForest @ [pair]
        let addTSqlSPPF pair = tsqlForest <- tsqlForest @ [pair]

        graphs
        |> ResizeArray.iter 
            (fun (lang, graph) ->
                match lang with
                | Calc -> 
                    calcXmlPath <- Calc.xmlPath
                    processLang graph Calc.tokenize Calc.parse lexerErrors.Add  errorCalc Calc.translate Calc.printAstToDot addCalcSPPF
                | JSON -> 
                    jsonXmlPath <- JSON.xmlPath
                    processLang graph JSON.tokenize JSON.parse lexerErrors.Add  errorJSON JSON.translate JSON.printAstToDot addJsonSPPF
                | TSQL -> 
                    tsqlXmlPath <- TSQL.xmlPath
                    processLang graph TSQL.tokenize TSQL.parse lexerErrors.Add  errorTSQL TSQL.translate TSQL.printAstToDot addTSqlSPPF
            )
        lexerErrors, parserErrors

    member this.XmlPath = 
        match currentLang with
        | Calc -> calcXmlPath 
        | JSON -> jsonXmlPath 
        | TSQL -> tsqlXmlPath
       // | _ -> System.String.Empty
    
    member this.CurrentLang = 
        match currentLang with
        | Calc -> "Calc"
        | JSON -> "JSON"
        | TSQL -> "TSQL"
       // | _ -> System.String.Empty
    
    member this.GetNextTree() = 
        let mutable res = null
        
        match currentLang with
        | Calc -> 
                res <- getNextTree calcForest Calc.translate
                if res = null
                then currentLang <- JSON
        | _ -> ()
        
        match currentLang with
        | JSON when res = null -> 
                res <- getNextTree jsonForest JSON.translate
                if res = null
                then currentLang <- TSQL
        | _ -> ()

        match currentLang with 
        | TSQL when res = null ->
                res <- getNextTree tsqlForest TSQL.translate
        | _ -> ()
                
        res

    member this.GetForestWithToken range (lang : string) = 
        match lang.ToLower() with
        | "calc" -> getForestWithToken calcForest range Calc.AbstractParser.tokenData Calc.translate
        | "json" -> getForestWithToken jsonForest range JSON.Parser.tokenData JSON.translate 
        | "tsql" -> getForestWithToken tsqlForest range tokenData TSQL.translate 
        | _ -> new ResizeArray<_>()