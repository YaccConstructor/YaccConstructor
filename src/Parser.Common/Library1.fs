namespace Yard.Generators.GLL.ParserCommon

open System.Collections.Generic
open Yard.Generators.Common.ASTGLL
open FSharpx.Collections.Experimental

[<Measure>] type vertexMeasure
[<Measure>] type nodeMeasure
[<Measure>] type labelMeasure

[<Struct>]
type Vertex =
    val Level            : int
    val NontermLabel     : int
    new (level, nonterm) = {Level = level; NontermLabel = nonterm}

[<Struct>]
type Context =
    val Index         : int
    val Label         : int<labelMeasure>
    val Vertex        : Vertex
    val Ast           : int<nodeMeasure>
    new (index, label, vertex, ast) = {Index = index; Label = label; Vertex = vertex; Ast = ast}


type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string


module CommonFuns = 

    let inline pack left right : int64 =  ((int64 left <<< 32) ||| int64 right)
    let inline getRight (long : int64) = int <| ((int64 long) &&& 0xffffffffL)
    let inline getLeft (long : int64)  = int <| ((int64 long) >>> 32)

    let inline packVertex level label: int64<vertexMeasure> =  LanguagePrimitives.Int64WithMeasure ((int64 level <<< 32) ||| int64 label)
    let inline getIndex1Vertex (long : int64<vertexMeasure>)       = int <| ((int64 long) &&& 0xffffffffL)
    let inline getIndex2Vertex (long : int64<vertexMeasure>)       = int <| ((int64 long) >>> 32)
 

    let inline packLabel rule position = ((int rule <<< 16) ||| int position)*1<labelMeasure>
    let inline getRule (packedValue : int<labelMeasure>)  = int packedValue >>> 16
    let inline getPosition (packedValue : int<labelMeasure>) = int (int packedValue &&& 0xffff)

type ParserStructures (inputLength : int) =
    let sppfNodes = new BlockResizeArray<INode>()
    let dummyAST = new TerminalNode(-1, packExtension -1 -1)
    
    let setP = new Dictionary<int64, ResizeArray<int<nodeMeasure>>> ()
    let epsilonNode = new TerminalNode(-1, packExtension 0 0)
    let setR = new Queue<Context>()  

    let getNodeP findSppfNode (findSppfPackedNode : int<nodeMeasure> -> int<labelMeasure> -> int64<extension> -> int64<extension> -> INode -> INode -> int<nodeMeasure>) dummy (label : int<labelMeasure>) (left : int<nodeMeasure>) (right : int<nodeMeasure>) : int<nodeMeasure> =
            let currentRight = sppfNodes.Item (int right)
            let rightExt = 
                match currentRight with                    
                    | :? NonTerminalNode as nonTerm ->
                        nonTerm.Extension
                    | :? IntermidiateNode as interm ->
                        interm.Extension
                    | :? TerminalNode as term ->
                        term.Extension   
                    | _ -> failwith "Smth strange, Nastya"             
            if left <> dummy
            then
                let currentLeft = sppfNodes.Item (int left)
                let leftExt =
                    match currentLeft with                    
                    | :? NonTerminalNode as nonTerm ->
                        nonTerm.Extension
                    | :? IntermidiateNode as interm ->
                        interm.Extension
                    | :? TerminalNode as term ->
                        term.Extension 
                    | _ -> failwith "Smth strange, Nastya" 
                    
                let y = findSppfNode label (getLeftExtension leftExt) (getRightExtension rightExt)
                ignore <| findSppfPackedNode y label leftExt rightExt currentLeft currentRight
                y
            else
                let y = findSppfNode label (getLeftExtension rightExt) (getRightExtension rightExt)
                ignore <| findSppfPackedNode y label rightExt rightExt dummyAST currentRight 
                y
    member this.GetNodeP = getNodeP
    member this.SetP = setP
    member this.EpsilonNode = epsilonNode
    member this.SetR = setR
    member this.SppfNodes = sppfNodes
    member this.DummyAST = dummyAST
        