module YC.SDK.CommonInterfaces

open Microsoft.FSharp.Collections
open System.Collections.Generic

open QuickGraph

open Yard.Utils.StructClass
open AbstractAnalysis.Common
open ControlFlowGraph
open Yard.Generators.RNGLR.OtherSPPF
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode

open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa

let lexerErrMsg = "Unexpected symbol:"
let parserErrMsg = "Syntax error. Unexpected token"
let undefinedVariableErrMsg = "Undefined variable"

type DrawingGraph (vertices : IEnumerable<int>, edges : ResizeArray<TaggedEdge<int, string>>) =
    member this.Vertices = vertices
    member this.Edges = edges

type LexingFinishedArgs<'node> (tokens : ResizeArray<'node>, lang : string, drawGraph : DrawingGraph) =
     inherit System.EventArgs()
     member this.Tokens = tokens
     member this.Lang = lang
     member this.Graph = drawGraph

type ParsingFinishedArgs(lang : string) = 
    inherit System.EventArgs()
    member this.Lang = lang

(*type TreeGenerationState<'node> = 
    | Start
    | InProgress of 'node * AstNode list
    | End of 'node*)

exception LexerException of string * obj

type InjectedLanguageAttribute(language : string) = 
    inherit System.Attribute()

    member this.Language = language

[<Interface>]
type IInjectedLanguageModule<'br, 'range, 'node when 'br : equality> =
     abstract Name: string
     abstract LexingFinished: IEvent<LexingFinishedArgs<'node>>
     abstract ParsingFinished: IEvent<ParsingFinishedArgs>
     abstract TokenNames : seq<string>
     //abstract GetNextTree: int -> 'node * bool
     //abstract GetForestWithToken: 'range -> ResizeArray<'node>
     abstract GetPairedRanges: int -> int -> 'range -> bool -> ResizeArray<'range>
     abstract Process
        : FSA<char * Position<'br>> -> 
            ResizeArray<string * 'range> * ResizeArray<string * 'range> * ResizeArray<string * 'range>


type Processor<'TokenType, 'br, 'range, 'node >  when 'br: equality and  'range: equality and 'node: null  and 'TokenType: equality
    (
        tokenize: FSA<char * Position<'br>> -> Test<ParserInputGraph<'TokenType>, array<Symb<char*Position<'br>>>>
        , parse, translate, tokenToNumber: 'TokenType -> int, numToString: int -> string
        , tokenData: 'TokenType -> obj, tokenToTreeNode, lang
        , calculatePos: _ -> seq<'range>
        , getDocumentRange: Position<'br> -> 'range
        , printAst: Tree<'TokenType> -> string -> unit
        , printOtherAst: OtherTree<'TokenType> -> string -> unit
        , semantic : _ option) = //as this =

    let lexingFinished = new Event<LexingFinishedArgs<'node>>()
    let parsingFinished = new Event<ParsingFinishedArgs>()
    
    let mutable forest: list<Tree<'TokenType> * _> = [] 
    let mutable otherForest : list<OtherTree<'TokenType>> = []

    //let mutable generationState : TreeGenerationState<'node> = Start
    
    ///creates instance of LexingFinishedArgs
    let prepareToTrigger (graph : ParserInputGraph<'TokenType>) tokenToTreeNode = 
        
        /// returns graph which will be later shown to user
        let createDrawGraph (graph : ParserInputGraph<_>) = 
            let edges = 
                graph.Edges
                |> Seq.map 
                    (
                        fun edge -> 
                            let tokenName = edge.Tag |> (tokenToNumber >> numToString)
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
                            Smbl (char, position) -> (sprintf "%s %c" lexerErrMsg char), position |> getDocumentRange 
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
                        #if DEBUG
                        //sometimes it needs for debugging purposes
                        printAst tree "result ast.dot"
                        #endif
                        
                        let pSource, lSource, tokToSourceString = semantic.Value
                        let cfg = new ControlFlow<'TokenType>(tree, pSource, lSource, tokToSourceString)
                        
                        #if DEBUG
                        //sometimes it needs for debugging purposes
                        cfg.PrintToDot "result cfg.dot"
                        #endif
                        let semErrors = cfg.FindUndefVariable()
                        semErrors |> List.iter (fun error -> addSError undefinedVariableErrMsg error)
                    

                | Yard.Generators.ARNGLR.Parser.Error(_, token, _) -> 
                    if token <> Unchecked.defaultof<'TokenType>
                    then token |> addPError 
            )

    (*let getNextTree index = 
        if forest.Length <= index
        then
            generationState <- End(null)
        else
            let mutable curSppf, errors = List.nth forest index
            let unprocessed = 
                match generationState with
                | Start -> List.init curSppf.TokensCount (fun i -> new Terminal(i))
                | InProgress (_, unproc) -> unproc |> List.map(fun n -> n :?> Terminal) 
                | _ -> failwith "Unexpected state in treeGeneration"
                
            let nextTree, unproc = curSppf.GetNextTree unprocessed (fun _ -> true)
            
            let treeNode = this.TranslateToTreeNode nextTree errors
            
            if unproc.IsEmpty
            then generationState <- End(treeNode)
            else generationState <- InProgress(treeNode, unproc |> List.map (fun n -> n :> AstNode)) 

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
        | _ -> failwith "Unexpected state in tree generation"*)

    member this.TokenToPos calculatePos (token : 'TokenType) = 
        let data (*: FSA<char*Position<'br>>*) = unbox <| tokenData token
        calculatePos data

    //member this.GetForestWithToken range = getForestWithToken range

    member this.GetPairedRanges leftNumber rightNumber range toRight = 
        
        let tokToPos' = this.TokenToPos calculatePos
        let info = new BracketSearchInfo<_>(leftNumber, rightNumber, range, toRight)
        
        let findPairs (tree : OtherTree<_>) = 
            tree.FindAllPair info tokenToNumber tokToPos'

        otherForest
        |> List.map (findPairs >> ResizeArray.map tokToPos' >> Seq.concat)
        |> Seq.concat
        |> ResizeArray.ofSeq

    //member this.TranslateToTreeNode nextTree errors = (Seq.head <| translate nextTree errors)
    
    member this.Process (graph : FSA<char * Position<'br>>) = 
        let lexerErrors = new ResizeArray<_>()
        let parserErrors = new ResizeArray<_>()
        let semanticErrors = new ResizeArray<_>()

        let addParserError errorMsg br tokenName = 
            parserErrors.Add <| (sprintf "%s" tokenName, br)

        let addSemanticError errorMsg br tokenName  = 
            semanticErrors.Add <| (sprintf "%s %s" errorMsg tokenName, br)

        let addError updateFun message token = 
            let tokenName = token |> (tokenToNumber >>  numToString)
            let tokenData = tokenData token :?> FSA<char * Position<'br>>
            tokenData 
            |> calculatePos
            |> Seq.iter
                ///TODO!!! Produce user friendly error message!
                (fun br -> updateFun message br tokenName)
        
        let addPError = addError addParserError parserErrMsg
        let addSError = addError addSemanticError
        
        processLang graph lexerErrors.Add addPError addSError

        lexerErrors, parserErrors, semanticErrors

    [<CLIEvent>]
    member this.LexingFinished = lexingFinished.Publish

    [<CLIEvent>]
    member this.ParsingFinished = parsingFinished.Publish