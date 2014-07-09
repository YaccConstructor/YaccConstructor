//link with Yacc

module Helper

open YC.ReSharper.AbstractAnalysis.Plugin.Core

type TreeGenerationState = 
    | Start
    | InProgress of ITreeNode * int list
    | End of ITreeNode

type Helper()=

    let mutable generationState : TreeGenerationState = Start
    
    let generateTreeNode (graphOpt : AbstractParsing.Common.ParserInputGraph<'token> option) tokenToTreeNode = 
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

            lexingFinished.Trigger(new LexingFinishedArgs(tokensList))

    let processLang graph tokenize parse addLError addPError translate addSPPF tokenToTreeNode = 
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
        
        generateTreeNode tokenizedGraph tokenToTreeNode 

        tokenizedGraph 
        |> Option.map parse
        |> Option.iter
            (function 
             | Yard.Generators.RNGLR.Parser.Success (sppf, _,errors) -> addSPPF (sppf, errors)
//                parsingFinished.Trigger (new ParsingFinishedArgs (lang))
             | Yard.Generators.RNGLR.Parser.Error(_,tok,_,_,errors) -> tok |> Array.iter addPError 
            )

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
            
            let treeNode = Processor.TranslateToTreeNode translate nextTree errors
            
            if unproc.IsEmpty
            then generationState <- End (treeNode)
            else generationState <- InProgress (treeNode, unproc) 

        generationState

    let getForestWithToken (forest : list<Yard.Generators.RNGLR.AST.Tree<'TokenType> * _>) 
                            range (tokenData: 'TokenType -> obj)  translate = 
        let res = new ResizeArray<_>()
        for ast, errors in forest do
            let trees = ast.GetForestWithToken range <| Processor.TokenToPos tokenData
            for tree in trees do
                let treeNode = Processor.TranslateToTreeNode translate tree errors
                res.Add treeNode
        res

    member this.GetNextTree lang index = 
        let state forest translate index = 
            getNextTree forest translate index
//            match lang with
//            | Calc -> getNextTree calcForest Calc.translate index //Calc.printAstToDot
//            | JSON -> getNextTree jsonForest JSON.translate index //JSON.printAstToDot
//            | TSQL -> getNextTree tsqlForest TSQL.translate index //TSQL.printAstToDot
        
        match state with
        | InProgress (treeNode, _) -> treeNode, false
        | End (treeNode) -> 
            generationState <- Start
            treeNode, true
        | _ -> failwith "Unexpected state in tree generation"

    member this.StringToLang (str : string) = 
        match str.ToLower() with
//        | "calc" -> Calc
//        | "json" -> JSON
//        | "tsql" -> TSQL
        | _ -> failwith "Unexpected language"

    member this.LangToString lang = 
        match lang with
//        | Calc -> "calc"
//        | JSON -> "json"
//        | TSQL -> "tsql"
        | _ -> failwith "calc"

    member this.XmlPath lang = "XML.PATH"
//        match lang with
//        | Calc -> Calc.xmlPath
//        | JSON -> JSON.xmlPath
//        | TSQL -> TSQL.xmlPath

    member this.GetForestWithToken range (str : string) =
        let lang = this.StringToLang str 
        let tokenData = fun _ -> ()
        fun translate = fun _ -> default(ITreeNode)
        let forest = new ResizeArray<_>()
        match lang with
        | _ -> getForestWithToken forest range tokenData translate
//        | JSON -> getForestWithToken jsonForest range JSON.Parser.tokenData JSON.translate 
//        | TSQL -> getForestWithToken tsqlForest range tokenData TSQL.translate 
//        | _ -> new ResizeArray<_>()