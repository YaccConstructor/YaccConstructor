module YC.SDK.CommonInterfaces

open Microsoft.FSharp.Collections
open System.Collections.Generic

open QuickGraph
open QuickGraph.Algorithms

open AbstractAnalysis.Common
open ControlFlowGraph
open OtherSPPF
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open YC.FST.AbstractLexing.Interpreter
open YC.FST.GraphBasedFst
open YC.FSA.FsaApproximation
open YC.FSA.GraphBasedFsa

type DrawingGraph (vertices : IEnumerable<int>, edges : List<TaggedEdge<int, string>>) =
    member this.Vertices = vertices
    member this.Edges = edges

type LexingFinishedArgs<'node> (tokens : ResizeArray<'node>, lang:string, drawGraph : DrawingGraph) =
     inherit System.EventArgs()
     member this.Tokens = tokens
     member this.Lang = lang
     member this.Graph = drawGraph

type ParsingFinishedArgs(lang : string) = 
    inherit System.EventArgs()
    member this.Lang = lang

type TreeGenerationState<'node> = 
    | Start
    | InProgress of 'node * AstNode list
    | End of 'node


exception LexerError of string * obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.language = language

[<Interface>]
type IInjectedLanguageModule<'br,'range,'node when 'br : equality> =    
     abstract Name: string
     abstract LexingFinished: IEvent<LexingFinishedArgs<'node>>
     abstract ParsingFinished: IEvent<ParsingFinishedArgs>
     abstract XmlPath: string
     abstract GetNextTree: int -> 'node * bool
     abstract GetForestWithToken: 'range -> ResizeArray<'node>
     abstract GetPairedRanges: int -> int -> 'range -> bool -> ResizeArray<'range>
     abstract Process
        : FSA<char * Position<'br>> -> 
            ResizeArray<string * 'range> * ResizeArray<string * 'range> * ResizeArray<string * 'range>


type Processor<'TokenType, 'br, 'range, 'node >  when 'br:equality and  'range:equality and 'node:null  and 'TokenType : equality
    (
        tokenize: FSA<char * Position<'br>> -> Test<ParserInputGraph<'TokenType>, array<Symb<char*Position<'br>>>>
        , parse, translate, tokenToNumber: 'TokenType -> int, numToString: int -> string, tokenData: 'TokenType -> obj, tokenToTreeNode, lang, calculatePos:_->seq<'range>
        , getDocumentRange: Position<'br> -> 'range
        , printAst: Tree<'TokenType> -> string -> unit
        , printOtherAst: OtherTree<'TokenType> -> string -> unit
        , semantic : _ option) as this =

    let lexingFinished = new Event<LexingFinishedArgs<'node>>()
    let parsingFinished = new Event<ParsingFinishedArgs>()
    
    let mutable forest: list<Tree<'TokenType> * _> = [] 
    let mutable otherForest : list<OtherTree<'TokenType>> = []

    let mutable generationState : TreeGenerationState<'node> = Start
    
    ///creates instance of LexingFinishedArgs
    let prepareToTrigger (graph : ParserInputGraph<'TokenType>) tokenToTreeNode = 
        
        /// returns graph which will be later shown to user
        let createDrawGraph (graph : ParserInputGraph<_>) = 
            let edges = 
                graph.Edges
                |> Seq.map 
                    (
                        fun edge -> 
                            let tokenName = edge.Tag |> tokenToNumber |> numToString
                            new TaggedEdge<_, _>(edge.Source, edge.Target, tokenName)
                    )
                |> ResizeArray.ofSeq

            let vertices = graph.Vertices
            DrawingGraph(vertices, edges)
        
        ///creates treeNodes collection. Highlighting needs in this collection.
        let createTreeNodes (graph : ParserInputGraph<_>) = 
            graph.Edges
            |> Seq.map(fun edge -> tokenToTreeNode edge.Tag)
            |> ResizeArray.ofSeq
        
        let drawGraph = createDrawGraph graph
        let treeNodes = createTreeNodes graph
            
        new LexingFinishedArgs<'node>(treeNodes, lang, drawGraph)

    let processLang graph addLError addPError addSError =
        
        let tokenizedGraph = 
            match tokenize graph with
            | Success res -> res |> Some
            | Error errors -> 
                errors 
                |> Array.map 
                    (
                        function 
                            Smbl e -> fst e |> string, snd e |> getDocumentRange 
                            | e -> failwithf "Unexpected tokenization result: %A" e
                    )
                |> Array.iter addLError 
                None

        tokenizedGraph
        |> Option.iter 
            (
                fun graph -> 
                    let lexFinishedArgs = prepareToTrigger graph tokenToTreeNode 
                    lexingFinished.Trigger(lexFinishedArgs)
            )

        tokenizedGraph 
        |> Option.map
            (fun x -> 
                let y = parse x
                printfn "Tree: %A" y
                y)
        |> Option.iter
            (function 
                | Yard.Generators.ARNGLR.Parser.Success(tree) ->
                    forest <- (tree, new ErrorDictionary<'TokenType>()) :: forest

                    otherForest <- new OtherTree<'TokenType>(tree) :: otherForest
                    parsingFinished.Trigger(new ParsingFinishedArgs(lang))

                    if semantic.IsSome 
                    then
                        //sometimes it needs for debugging purposes
                        //printAst tree "result ast.dot"
                        let pSource, lSource, tokToSourceString = semantic.Value
                        let cfg = new ControlFlow<'TokenType>(tree, pSource, lSource, tree.Tokens, tokToSourceString)
                        //sometimes it needs for debugging purposes
                        //cfg.PrintToDot "result cfg.dot"
                        let semErrors = cfg.FindUndefVariable()
                        semErrors |> List.iter addSError
                    

                | Yard.Generators.ARNGLR.Parser.Error(_, tok, _) -> 
                    if tok <> Unchecked.defaultof<'TokenType>
                    then tok |> addPError 
            )

    let getNextTree index = 
        if forest.Length <= index
        then
            generationState <- End(null)
        else
            let mutable curSppf, errors = List.nth forest index
            let unprocessed = 
                match generationState with
                | Start ->   List.init curSppf.TokensCount (fun i -> new Terminal(i))
                | InProgress (_, unproc) -> unproc |> List.map (fun n -> n :?> Terminal) 
                | _ -> failwith "Unexpected state in treeGeneration"
                
            let nextTree, unproc = curSppf.GetNextTree unprocessed (fun _ -> true)
            
            let treeNode = this.TranslateToTreeNode nextTree errors
            
            if unproc.IsEmpty
            then generationState <- End (treeNode)
            else generationState <- InProgress (treeNode, unproc |> List.map (fun n -> n :> AstNode)) 

        generationState

    let getForestWithToken range = 

        forest
        |> List.map
            (
                fun (ast, errors) -> 
                    let trees = ast.GetForestWithToken range <| this.TokenToPos calculatePos
                    trees
                    |> List.map (fun tree -> this.TranslateToTreeNode tree errors)
            )
        |> List.concat
        |> ResizeArray.ofList

    member this.GetNextTree index =         
        let state = getNextTree 0//index
        match state with
        | InProgress (treeNode, _) -> treeNode, false
        | End (treeNode) -> 
            generationState <- Start
            treeNode, true
        | _ -> failwith "Unexpected state in tree generation"

    member this.TokenToPos calculatePos (token : 'TokenType) = 
        let data (*: FSA<char*Position<'br>>*) = unbox <| tokenData token
        calculatePos data

    member this.GetForestWithToken range = getForestWithToken range

    member this.GetPairedRanges leftNumber rightNumber range toRight = 
        
        let tokToPos = this.TokenToPos calculatePos
        
        otherForest
        |> List.map (fun otherTree -> otherTree.FindAllPair leftNumber rightNumber range toRight tokenToNumber tokToPos)
        |> List.map 
            (
                fun token -> 
                    token 
                    |> ResizeArray.map(fun token -> tokToPos token)
                    |> Seq.concat
            )
        |> Seq.concat
        |> ResizeArray.ofSeq


    member this.TranslateToTreeNode nextTree errors = (Seq.head <| translate nextTree errors)
    
    member this.Process (graph : FSA<char * Position<'br>>) = 
        let lexerErrors = new ResizeArray<_>()
        let parserErrors = new ResizeArray<_>()
        let semanticErrors = new ResizeArray<_>()

        let addError tok isParse =
            let e tokName (tokenData: FSA<char*Position<'br>>) = 
                tokenData 
                |> calculatePos
                |> Seq.iter
                    ///TODO!!! Produce user friendly error message!
                    (fun br -> if isParse 
                               then parserErrors.Add <| ((sprintf "%A" tokName), br)
                               else semanticErrors.Add <| ((sprintf "%A" tokName), br))
            let name = tok |> (tokenToNumber >>  numToString)
            let tokenData = tokenData tok :?> FSA<char*Position<'br>>
            e name tokenData
        
        let addPError tok = addError tok true
        let addSError tok = addError tok false
        
        processLang graph lexerErrors.Add addPError addSError

        lexerErrors, parserErrors, semanticErrors

    [<CLIEvent>]
    member this.LexingFinished = lexingFinished.Publish

    [<CLIEvent>]
    member this.ParsingFinished = parsingFinished.Publish