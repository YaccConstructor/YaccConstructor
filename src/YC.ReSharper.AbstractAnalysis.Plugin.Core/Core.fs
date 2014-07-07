namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

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
open QuickGraph
open QuickGraph.Algorithms

type SupportedLangs =
    | Calc
    | TSQL
    | JSON

type LexingFinishedArgs (tokens : ResizeArray<ITreeNode>, lang : string, xml : string) =
     inherit System.EventArgs()

     member this.Tokens = tokens
     member this.Lang = lang
     member this.Xml = xml

type Processor(file) =
    let mutable currentLang = Calc
    
    let mutable calcForest = []
    let mutable jsonForest = []
    let mutable tsqlForest = []

    ///Needs for tree generation for highlighting
    let mutable unprocessed = []
    let mutable count = 0

    let lexingFinished = new Event<LexingFinishedArgs>()

    let langToString lang = 
        match lang with
        | Calc -> "Calc"
        | JSON -> "JSON"
        | TSQL -> "TSQL"

    let langToXml lang = 
        match lang with
        | Calc -> Calc.xmlPath
        | JSON -> JSON.xmlPath
        | TSQL -> xmlPath
        | _ -> System.String.Empty
        
        
    let generateTreeNode (graphOpt : AbstractParsing.Common.ParserInputGraph<'token> option) tokenToTreeNode lang = 
        if graphOpt.IsSome
        then
            let inGraph = graphOpt.Value 
            let ts =  inGraph.TopologicalSort() |> Array.ofSeq
            let ids = dict (ts |> Array.mapi (fun i v -> v,i))
            let tokens = 
                ts
                |> Seq.mapi (fun i v -> inGraph.OutEdges v |> (Seq.map (fun e -> e.Tag(*, ids.[e.Target]*))) |> Array.ofSeq)
            let enumerator = tokens.GetEnumerator()
            let tokensList = new ResizeArray<_>()
            while enumerator.MoveNext() do
                let t = enumerator.Current
               // let tt = fst <| t.[0]
                t |> Array.iter (fun i -> 
                    let treeNode = tokenToTreeNode i
                    tokensList.Add treeNode)
//                tokensList.Add t
            lexingFinished.Trigger(new LexingFinishedArgs(tokensList, langToString lang, langToXml lang))
            ()
            //|> Seq.filter (fun (_,a) -> a.Length > 0)

    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | "objnotation" -> JSON
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let processLang graph tokenize parse addLError addPError translate (*printer*) addSPPF tokenToTreeNode (lang : SupportedLangs) = 
        let tokenize g =
            try 
               tokenize g
               |> Some 
            with
            | LexerError(t,brs) ->
                (t, (brs :?> array<AbstractLexer.Core.Position<ICSharpLiteralExpression>>).[0].back_ref.GetDocumentRange())
                |> addLError
                None

        let tokenizedGraph = tokenize graph 
        
        generateTreeNode tokenizedGraph tokenToTreeNode lang 

        tokenizedGraph 
        |> Option.map parse
        |> Option.iter
            (function 
             | Yard.Generators.RNGLR.Parser.Success (sppf, _,errors) -> 
                //printer sppf "ast.dot"
                addSPPF (sppf, errors)
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


    let getNextTree (forest : list<Yard.Generators.RNGLR.AST.Tree<'TokenType> * _>) translate (*printer*) = 
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
                    processLang graph Calc.tokenize Calc.parse lexerErrors.Add  errorCalc Calc.translate (*Calc.printAstToDot*) addCalcSPPF Calc.tokenToTreeNode lang
                | JSON -> 
                    processLang graph JSON.tokenize JSON.parse lexerErrors.Add  errorJSON JSON.translate (*JSON.printAstToDot*) addJsonSPPF JSON.tokenToTreeNode lang
                | TSQL -> 
                    processLang graph TSQL.tokenize TSQL.parse lexerErrors.Add  errorTSQL TSQL.translate (*TSQL.printAstToDot*) addTSqlSPPF TSQL.tokenToTreeNode lang
            )
        lexerErrors, parserErrors

    member private this.langToString lang = langToString lang
        

    member this.CurrentLang = this.langToString currentLang
    
    [<CLIEvent>]
    member this.LexingFinished = lexingFinished.Publish

    member this.GetNextTree() = 
        let mutable res = null
        
        match currentLang with
        | Calc -> 
                res <- getNextTree calcForest Calc.translate //Calc.printAstToDot
                if res = null
                then currentLang <- JSON
        | _ -> ()
        
        match currentLang with
        | JSON when res = null -> 
                res <- getNextTree jsonForest JSON.translate //JSON.printAstToDot
                if res = null
                then currentLang <- TSQL
        | _ -> ()

        match currentLang with 
        | TSQL when res = null ->
                res <- getNextTree tsqlForest TSQL.translate //TSQL.printAstToDot
        | _ -> ()
                
        res

    member this.XmlPath lang = langToXml lang
        

    member this.CurrentXmlPath = this.XmlPath currentLang
        
    member this.GetForestWithToken range (lang : string) = 
        match lang.ToLower() with
        | "calc" -> getForestWithToken calcForest range Calc.AbstractParser.tokenData Calc.translate
        | "json" -> getForestWithToken jsonForest range JSON.Parser.tokenData JSON.translate 
        | "tsql" -> getForestWithToken tsqlForest range tokenData TSQL.translate 
        | _ -> new ResizeArray<_>()