namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open Microsoft.FSharp.Collections
open QuickGraph
open QuickGraph.Algorithms
open YC.AbstractAnalysis.CommonInterfaces
open AbstractAnalysis.Common

open System
open Mono.Addins

type TreeGenerationState<'node> = 
    | Start
    | InProgress of 'node * int list
    | End of 'node

[<assembly:AddinRoot ("YC.ReSharper.AbstractAnalysis.Plugin.Core", "1.0")>]
do()


type LanguagesProcessor<'br,'range, 'node>() =
    do AddinManager.Initialize()
    do AddinManager.Registry.Update(null)    
    let injectedLanguages = 
        let an = new System.Reflection.AssemblyName();
        an.Name <- "Mono.Addins";
        an.Version <- new System.Version(1, 1, 0);
        //System.Reflection.Assembly.Load(an);
        let s = AddinManager.Registry.DefaultAddinsFolder
        let a = AddinManager.IsInitialized
        let r = AddinManager.Registry
        AddinManager.AddinLoadError.Add(
            fun t -> 
                let x = t.Message
                printfn "%A" x)        
        //let n = AddinManager.GetExtensionNodes (@"C:\gsv\projects\recursive-ascent\Bin\Debug\v40\Addins\")
        let d = new System.Collections.Generic.Dictionary<_,_>(System.StringComparer.InvariantCultureIgnoreCase)
        AddinManager.GetExtensionObjects (typeof<IInjectedLanguageModule<'br,'range, 'node>>) 
        |> Seq.cast<IInjectedLanguageModule<'br,'range, 'node>>
        |> Array.ofSeq
        |> Array.iter (fun x -> d.Add(x.Name,x))
                
        d

    member this.Process (graphs:ResizeArray<string*_>) =
        graphs
        |> ResizeArray.map(fun (l,g) -> injectedLanguages.[l].Process g)

    member this.LexingFinished =  injectedLanguages |> Seq.map (fun l -> l.Value.LexingFinished)
    member this.ParsingFinished = injectedLanguages |> Seq.map (fun l -> l.Value.ParsingFinished)
    member this.XmlPath l = injectedLanguages.[l].XmlPath
    member this.GetNextTree l i = injectedLanguages.[l].GetNextTree i
    member this.GetForestWithToken l rng = injectedLanguages.[l].GetForestWithToken rng

type Processor<'TokenType,'br, 'range, 'node>  when 'br:equality and  'range:equality and 'node:null
    (
        tokenize: LexerInputGraph<'br> -> ParserInputGraph<'TokenType>
        , parse, translate, tokenToNumber: 'TokenType -> int, numToString: int -> string, tokenData: 'TokenType -> obj, tokenToTreeNode, lang, calculatePos:_->seq<'range>
        , getDocumentRange:'br -> 'range) as this =

    let lexingFinished = new Event<LexingFinishedArgs<'node>>()
    let parsingFinished = new Event<ParsingFinishedArgs>()
    let mutable forest: list<Yard.Generators.RNGLR.AST.Tree<'TokenType> * _> = [] 

    let mutable generationState : TreeGenerationState<'node> = Start
    
    let generateTreeNode (graphOpt : ParserInputGraph<'token> option) tokenToTreeNode = 
        if graphOpt.IsSome
        then
            let inGraph = graphOpt.Value 
            let ts =  inGraph.TopologicalSort() |> Array.ofSeq
            let ids = dict (ts |> Array.mapi (fun i v -> v,i))
            let tokens = 
                ts
                |> Seq.mapi (fun i v -> inGraph.OutEdges v |> (Seq.map (fun e -> e.Tag)) |> Array.ofSeq)
            let enumerator = tokens.GetEnumerator()
            let tokensList = new ResizeArray<_>()
            while enumerator.MoveNext() do
                let t = enumerator.Current
                t |> Array.iter (fun i -> 
                    let treeNode = tokenToTreeNode i
                    tokensList.Add treeNode)

            lexingFinished.Trigger(new LexingFinishedArgs<'node>(tokensList, lang))

    let processLang graph addLError addPError =
        let tokenize g =
            try 
                tokenize g
                |> Some 
            with
            | LexerError(t,brs) ->
                (t, (brs :?> array<AbstractLexer.Core.Position<'br>>).[0].back_ref |> getDocumentRange)
                |> addLError
                None

        let tokenizedGraph = tokenize graph 
        
        generateTreeNode tokenizedGraph tokenToTreeNode 

        tokenizedGraph |> Option.map parse
        |> Option.iter
            (function 
                | Yard.Generators.RNGLR.Parser.Success(x,_,e) ->
                    forest <- (x,e) :: forest
                    parsingFinished.Trigger (new ParsingFinishedArgs (lang))
                | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )

    let getNextTree index : TreeGenerationState<'node> = 
        if forest.Length <= index
        then
            generationState <- End(null)
        else
            let mutable curSppf, errors = List.nth forest index
            let unprocessed = 
                match generationState with
                | Start ->              Array.init curSppf.TokensCount (fun i -> i) |> List.ofArray
                | InProgress (_, unproc) ->  unproc
                | _ -> failwith "Unexpected state in treeGeneration"
                
            let nextTree, unproc = curSppf.GetNextTree unprocessed (fun _ -> true)
            
            let treeNode = this.TranslateToTreeNode nextTree errors
            
            if unproc.IsEmpty
            then generationState <- End (treeNode)
            else generationState <- InProgress (treeNode, unproc) 

        generationState

    let getForestWithToken range = 
        let res = new ResizeArray<'node>()
        for ast, errors in forest do
            let trees = ast.GetForestWithToken range <| (this.TokenToPos calculatePos)
            for tree in trees do
                let treeNode = (this.TranslateToTreeNode tree errors)
                res.Add treeNode
        res
          
//    member this.CalculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =    
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

    member this.GetNextTree index =         
        let state = getNextTree 0//index
        match state with
        | InProgress (treeNode, _) -> treeNode, false
        | End (treeNode) -> 
            generationState <- Start
            treeNode, true
        | _ -> failwith "Unexpected state in tree generation"

    member this.TokenToPos calculatePos token = 
        let data = unbox <| tokenData token
        let str : string = fst data
        let pos : array<AbstractLexer.Core.Position<'br>> 
                = snd data
                
        //this.CalculatePos pos
        calculatePos pos

    member this.GetForestWithToken range =        
        getForestWithToken range

    member this.TranslateToTreeNode nextTree errors = (Seq.head <| translate nextTree errors)
    
    member this.Process (graph:LexerInputGraph<'br>) = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        let filterBrs (brs:array<AbstractLexer.Core.Position<'br>>) =
            let res = new ResizeArray<AbstractLexer.Core.Position<'br>>(3)
            brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref, br.back_ref)) |> not then res.Add br)
            res.ToArray()
                    
        let addError tok =
            let e t l (brs:array<AbstractLexer.Core.Position<'br>>) = 
                brs |> calculatePos
                |> Seq.iter
                    (fun br -> parserErrors.Add <| ((sprintf "%A(%A)" t l), br))
            let name = tok |> (tokenToNumber >>  numToString)
            let (l:string),br = tokenData tok :?> _
            e name l br
        
        processLang graph lexerErrors.Add addError

        lexerErrors, parserErrors

    [<CLIEvent>]
    member this.LexingFinished = lexingFinished.Publish

    [<CLIEvent>]
    member this.ParsingFinished = parsingFinished.Publish