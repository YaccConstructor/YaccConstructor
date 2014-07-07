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

type TreeGenerationState = 
    | Start
    | InProgress of ITreeNode * int list
    | End of ITreeNode

type LexingFinishedArgs (tokens : ResizeArray<ITreeNode>, lang : SupportedLangs) =
     inherit System.EventArgs()

     member this.Tokens = tokens
     member this.Lang = lang

type ParsingFinishedArgs((*sppf : Yard.Generators.RNGLR.AST.Tree<'TokenType> * System.Collections.Generic.Dictionary<_,_>, *)lang : SupportedLangs) = 
    inherit System.EventArgs()
//
//    member this.Sppf = sppf
    member this.Lang = lang

type Processor(file) =
    let mutable currentLang = Calc
    
    let mutable calcForest = []
    let mutable jsonForest = []
    let mutable tsqlForest = []

    let lexingFinished = new Event<LexingFinishedArgs>()
    let parsingFinished = new Event<ParsingFinishedArgs>()

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
                t |> Array.iter (fun i -> 
                    let treeNode = tokenToTreeNode i
                    tokensList.Add treeNode)

            lexingFinished.Trigger(new LexingFinishedArgs(tokensList, lang))

    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> TSQL
            | "eval" -> Calc
            | "objnotation" -> JSON
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"
    let processLang graph tokenize parse addLError addPError translate addSPPF tokenToTreeNode (lang : SupportedLangs) = 
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
                parsingFinished.Trigger (new ParsingFinishedArgs (lang))
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

    let mutable generationState : TreeGenerationState = Start
    
    let getNextTree (forest : list<Yard.Generators.RNGLR.AST.Tree<'TokenType> * _>) translate index = 
        if forest.Length <= index
        then
            generationState <- End (null)
        else
            let mutable curSppf, errors = List.nth forest index
            let unprocessed = 
                match generationState with
                | Start ->              Array.init curSppf.TokensCount (fun i -> i) |> List.ofArray
                | InProgress (_, unproc) ->  unproc
                | _ -> failwith "Unexpected state in treeGeneration"
                
            let nextTree, unproc = curSppf.GetNextTree unprocessed (fun _ -> true)
            
            let treeNode = (Seq.head <| translate nextTree errors) :> ITreeNode
            
            if unproc.IsEmpty
            then generationState <- End (treeNode)
            else generationState <- InProgress (treeNode, unproc) 

        generationState
                

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
                    processLang graph Calc.tokenize Calc.parse lexerErrors.Add  errorCalc Calc.translate addCalcSPPF Calc.tokenToTreeNode lang
                | JSON -> 
                    processLang graph JSON.tokenize JSON.parse lexerErrors.Add  errorJSON JSON.translate addJsonSPPF JSON.tokenToTreeNode lang
                | TSQL -> 
                    processLang graph TSQL.tokenize TSQL.parse lexerErrors.Add  errorTSQL TSQL.translate addTSqlSPPF TSQL.tokenToTreeNode lang
            )
        lexerErrors, parserErrors

    [<CLIEvent>]
    member this.LexingFinished = lexingFinished.Publish

    [<CLIEvent>]
    member this.ParsingFinished = parsingFinished.Publish

    member this.GetNextTree lang index = 
//        let mutable res = null
        
        let state = 
            match lang with
            | Calc -> getNextTree calcForest Calc.translate index //Calc.printAstToDot
            | JSON -> getNextTree jsonForest JSON.translate index //JSON.printAstToDot
            | TSQL -> getNextTree tsqlForest TSQL.translate index //TSQL.printAstToDot
        
        match state with
        | InProgress (treeNode, _) -> treeNode, false
        | End (treeNode) -> 
            generationState <- Start
            treeNode, true
        | _ -> failwith "Unexpected state in tree generation"

    member this.StringToLang (str : string) = 
        match str.ToLower() with
        | "calc" -> Calc
        | "json" -> JSON
        | "tsql" -> TSQL
        | _ -> failwith "Unexpected language"

    member this.LangToString lang = 
        match lang with
        | Calc -> "calc"
        | JSON -> "json"
        | TSQL -> "tsql"

    member this.XmlPath lang = 
        match lang with
        | Calc -> Calc.xmlPath
        | JSON -> JSON.xmlPath
        | TSQL -> TSQL.xmlPath

    member this.GetForestWithToken range (str : string) =
        let lang = this.StringToLang str 
        match lang with
        | Calc -> getForestWithToken calcForest range Calc.AbstractParser.tokenData Calc.translate
        | JSON -> getForestWithToken jsonForest range JSON.Parser.tokenData JSON.translate 
        | TSQL -> getForestWithToken tsqlForest range tokenData TSQL.translate 
        | _ -> new ResizeArray<_>()