module Yard.Generators.RIGLR.Parser 

open System.Collections.Generic

open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
//open QuickGraph

type RCGVertex =
    struct
        val label: int
        val outEdges: RCGEdge list
        new (l, edges) = {label = l; outEdges = edges}
    end

and RCGEdge =
    struct
        val pointer: int
        val dest: RCGVertex
        new (p, d) = {pointer = p; dest = d}
    end    

//let RCG = new AdjacencyGraph<int*int, TaggedEdge<int*int, int>>()

type ParseResult<'TokenType> =
    | Success of Tree<'TokenType>
    | Error of string

let buildAst<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) =
    let tokenEnum = tokens.GetEnumerator()
    let verticies = new ResizeArray<RCGVertex>()  // RCG with base node    
    let pointerToNodes = new Dictionary<int, int[]>()

    // Node*label (start position in the input that the yield of the sub-graph derives)
    let sppfNodes = new ResizeArray<AstNode*int>()
        
    let curPointer = ref 0
    let curToken = ref tokenEnum.Current
    let curNum = ref (parserSource.TokenToNumber tokenEnum.Current)
    let curStepProcesses = new ResizeArray<int*int*int>()  // (RCA state, RCG state, pointer)    
    let setP = new ResizeArray<int*int[]>()
    let curStepNodes = new ResizeArray<_>()
    
    //curProcesses.Add (0, 0, -1)    
    while !curNum <> parserSource.EofIndex do
        ()

    box()
    
    
