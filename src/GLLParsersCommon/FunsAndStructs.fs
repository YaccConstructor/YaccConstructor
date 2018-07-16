namespace Yard.Generators.GLL.ParserCommon

open AbstractAnalysis.Common
open Yard.Generators.Common.ASTGLL
open FSharpx.Collections.Experimental
open System.Collections.Generic
open Yard.Generators.Common.DataStructures

module CommonFuns = 

    let inline pack left right : int64 = ((int64 left <<< 32) ||| int64 right)
    let inline pack3 l m r : int64 =  ((int64 l <<< 42) ||| (int64 m <<< 21) ||| int64 r)
    let inline getRight (long : int64) = int <| ((int64 long) &&& 0xffffffffL)
    let inline getLeft (long : int64)  = int <| ((int64 long) >>> 32)

    let inline packVertex level label: int64<gssVertex>  =  LanguagePrimitives.Int64WithMeasure ((int64 level <<< 32) ||| int64 label)
    let inline getIndex1Vertex (long : int64<gssVertex>) = int <| ((int64 long) &&& 0xffffffffL)
    let inline getIndex2Vertex (long : int64<gssVertex>) = int <| ((int64 long) >>> 32)

    let inline packVertexFSA position state: int64<gssVertex>  = LanguagePrimitives.Int64WithMeasure ((int64 position <<< 32) ||| int64 state)
    let inline getPosition (packed : int64<gssVertex>) = int <| ((int64 packed) &&& 0xffffffffL)
    let inline getState (packed : int64<gssVertex>) = int <| ((int64 packed) >>> 32)

    let inline packEdgePos edge position : int<positionInGrammar>  =
        if (edge < 65536) && (position < 65536) then LanguagePrimitives.Int32WithMeasure((int position <<< 16) ||| int edge)
        else failwith "Edge or position is greater then 65535!!"
    let inline getEdge (packedValue : int<positionInInput>)      = int (int packedValue &&& 0xffff)
    let inline getPosOnEdge (packedValue : int<positionInInput>) = int (uint32 packedValue >>> 16)

    let inline packLabelNew rule position : int<positionInGrammar>   = LanguagePrimitives.Int32WithMeasure((int rule <<< 16) ||| int position)                               
    let inline getRuleNew (packedValue : int<positionInGrammar>)     = int packedValue >>> 16
    let inline getPositionNew (packedValue : int<positionInGrammar>) = int (int packedValue &&& 0xffff)

[<Struct>]
[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Pack = 1)>]
type Vertex =
    /// Position in input graph (Packed edge+position)
    val Level            : int<positionInGrammar>
    /// Nonterminal
    val NontermLabel     : int<positionInGrammar>
    new (level, nonterm) = {Level = level; NontermLabel = nonterm}

[<Struct>]
[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Pack = 1)>]
type GSSVertexFSA =
    /// Position in input graph (Packed edge+position)
    val PositionInInput  : int<positionInInput>
    /// Nonterminal
    val Nonterm     : int<positionInGrammar>
    new (positionInInput, nonterm) = {PositionInInput = positionInInput; Nonterm = nonterm}

[<Struct>]
type Context(*<'TokenType>*) =
    val Index         : int
    val Label         : int<positionInGrammar>
    val Vertex        : Vertex
    val Ast           : int<nodeMeasure>
    val Probability   : float
    val SLength       : int   
    //val Path          : List<ParserEdge<'TokenType*ref<bool>>>
    new (index, label, vertex, ast, prob, sLength) = {Index = index; Label = label; Vertex = vertex; Ast = ast; Probability = prob; SLength = sLength} // Path = List.empty<ParserEdge<'TokenType*ref<bool>>>
    new (index, label, vertex, ast) = {Index = index; Label = label; Vertex = vertex; Ast = ast; Probability = 1.0; SLength = 1}
    //new (index, label, vertex, ast, path) = {Index = index; Label = label; Vertex = vertex; Ast = ast; Path = path}

type ParseData = 
    | TreeNode of int<nodeMeasure>
    | Length of uint16

[<Struct>]
[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Pack = 1)>]
type ContextFSA<'GSSVertex> =
    /// Position in input graph (packed edge+position).
    val PosInInput         : int<positionInInput>
    /// Current state of FSA.
    val PosInGrammar         : int<positionInGrammar>
    /// Current GSS node.
    val GssVertex        : 'GSSVertex
    /// 4 values packed in one int64: leftEdge, leftPos, rightEdge, rightPos.
    //val LeftPos       : int<leftPosition>
    /// Length of current result
    val Data        : ParseData
    new (index, state, vertex, data) = {PosInInput = index; PosInGrammar = state; GssVertex = vertex; Data = data}
    override this.ToString () = "Edge:" + (CommonFuns.getEdge(this.PosInInput).ToString()) +
                                "; PosOnEdge:" + (CommonFuns.getPosOnEdge(this.PosInInput).ToString()) +
                                "; State:" + (this.PosInGrammar.ToString()) +
                                //"; LeftPos:" + (this.LeftPos.ToString()) +
                                "; Len:" + (this.Data.ToString())

[<Struct>]
[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Pack = 1)>]
type ContextFSA =
    /// Position in input.
    val Index         : int<positionInInput>
    /// Current state of FSA.
    val State         : int<positionInGrammar>
    /// Current GSS node.
    val Vertex        : GSSVertexFSA
    val CurrentN      : int<nodeMeasure>
    new (index, state, vertex, currentN) = {Index = index; State = state; Vertex = vertex; CurrentN = currentN}
    override this.ToString () = "Edge:" + (CommonFuns.getEdge(this.Index).ToString()) +
                                "; PosOnEdge:" + (CommonFuns.getPosOnEdge(this.Index).ToString()) +
                                "; State:" + (this.State.ToString())

type ParseResult<'a> =
    | Success of Tree<'a>
    | Success1 of 'a[]
    | Error of string

type TypeOfNode = 
    | Nonterm of int<positionInGrammar>
    | Intermed of int<positionInGrammar> * int<positionInGrammar>

[<Struct>]
type ResultStruct =
    val le : int
    val lpos : int
    val re : int
    val rpos : int
    val length : uint16
    new (l, l1, r, r1, len) = {le = l; lpos = l1; re = r; rpos = r1; length = len}
    override this.ToString () = "Start:edge:" + (this.le.ToString()) + ";pos:" + (this.lpos.ToString()) + "--" +
                                "Final:edge:" + (this.re.ToString()) + ";pos:" + (this.rpos.ToString())

type CompressedArray<'t>(l : int[], f : _ -> 't, shift) =
    let a = Array.init l.Length (fun i -> Array.init (l.[i]) f)
    member this.Item         
        with get (i:int<positionInInput>) = 
            let edg = (CommonFuns.getEdge i)
            let pos = (CommonFuns.getPosOnEdge i)
            a.[edg].[shift + pos]
        and set i v = a.[(CommonFuns.getEdge i)].[shift + (CommonFuns.getPosOnEdge i)] <- v


type ParserStructures<'TokenType> (currentRule : int)=
    let sppfNodes = new BlockResizeArray<INode>()
    let dummyAST = new TerminalNode(-1, packExtension -1 -1)
    let setP = new Dictionary<int64, Yard.Generators.Common.DataStructures.ResizableUsualOne<int<nodeMeasure>>>(500)//list<int<nodeMeasure>>> (500)
    let epsilonNode = new TerminalNode(-1, packExtension 0 0)
    let setR = new System.Collections.Generic. Queue<Context>(100)  
    let dummy = 0<nodeMeasure>
    let currentN = ref <| dummy
    let currentR = ref <| dummy
    let resultAST = ref None
    do 
        sppfNodes.Add dummyAST
        sppfNodes.Add epsilonNode

    let currentLabel = ref <| (CommonFuns.packLabelNew currentRule 0)
    
    let getTreeExtension (node : int<nodeMeasure>) =
        match sppfNodes.Item (int node) with
        | :? TerminalNode as t ->
            t.Extension
        | :? IntermidiateNode as i ->
            i.Extension
        | :? NonTerminalNode as n ->
            n.Extension
        | _ -> failwith "Bad type for tree node"   

    let getNodeP 
        findSppfNode 
        (findSppfPackedNode : _ -> _ -> _ -> _ -> INode -> INode -> int<nodeMeasure>) 
        dummy (label : int<positionInGrammar>) (left : int<nodeMeasure>) (right : int<nodeMeasure>) : int<nodeMeasure> =
            let currentRight = sppfNodes.Item (int right)  
            let rightExt = getTreeExtension right           
            if left <> dummy
            then
                let currentLeft = sppfNodes.Item (int left)
                let leftExt = getTreeExtension left
                let y = findSppfNode label (getLeftExtension leftExt) (getRightExtension rightExt)
                ignore <| findSppfPackedNode y label leftExt rightExt currentLeft currentRight
                y
            else
                let y = findSppfNode label (getLeftExtension rightExt) (getRightExtension rightExt)
                ignore <| findSppfPackedNode y label rightExt rightExt dummyAST currentRight 
                y
      //CompressedArray<Dictionary<_, Dictionary<_, ResizeArray<_>>>>                           
    let containsContext (setU : Dictionary<_, Dictionary<_, ResizeArray<_>>>[]) inputIndex (label : int<positionInGrammar>) (vertex : Vertex) (ast : int<nodeMeasure>) =
        let vertexKey = CommonFuns.pack vertex.Level vertex.NontermLabel
        if setU.[inputIndex] <> Unchecked.defaultof<_>
        then
            let cond, current = setU.[inputIndex].TryGetValue(int label) 
            if  cond
            then
                if current.ContainsKey vertexKey
                then
                    let trees = current.[vertexKey]
                    if not <| trees.Contains ast
                    then 
                        trees.Add ast
                        false
                    else
                        true
                else 
                    let arr = new ResizeArray<int<nodeMeasure>>()
                    arr.Add ast
                    current.Add(vertexKey, arr)                    
                    false
            else 
                let dict = new Dictionary<_, ResizeArray<_>>()
                setU.[inputIndex].Add(int label, dict)
                let arr = new ResizeArray<int<nodeMeasure>>()
                arr.Add ast
                dict.Add(vertexKey, arr) 
                false
        else 
            let dict1 = new Dictionary<_, _>()
            setU.[inputIndex] <- dict1
            let dict2 = new Dictionary<_, ResizeArray<_>>()
            dict1.Add(int label, dict2)
            let arr = new ResizeArray<int<nodeMeasure>>()
            arr.Add ast
            dict2.Add(vertexKey, arr)
            false
        //else true
//CompressedArray<System.Collections.Generic.Dictionary<_, System.Collections.Generic.Dictionary<_, ResizeArray<_>>>>
    let addContext (setU ) (inputVertex : int) (label : int<positionInGrammar>) vertex ast =
        setR.Enqueue(new Context(inputVertex, label, vertex, ast(*, curR*)))
        
        if not <| containsContext setU inputVertex label vertex ast
        then
            setR.Enqueue(new Context(inputVertex, label, vertex, ast(*, dummy, currentPath*)))

    let containsEdge (dict1 : Dictionary<_, Dictionary<_, ResizeArray<_>>>) ast (e : Vertex) =
        if dict1 <> Unchecked.defaultof<_>
            then
                if dict1.ContainsKey(ast)
                then
                    let dict2 = dict1.[ast]
                    if dict2.ContainsKey(e.NontermLabel)
                    then
                        let t = dict2.[e.NontermLabel]
                        if t.Contains(e.Level) 
                        then true, None
                        else 
                            t.Add(e.Level) 
                            false, None 
                    else
                        let arr = new ResizeArray<_>()
                        arr.Add(e.Level) 
                        dict2.Add(e.NontermLabel, arr)
                        false, None
                else
                    let d = new Dictionary<_, ResizeArray<_>>()
                    dict1.Add(ast, d)
                    let l = new ResizeArray<_>()
                    l.Add(e.Level)
                    d.Add(e.NontermLabel, l)
                    false, None
            else
                let newDict1 = new Dictionary<int<nodeMeasure>, Dictionary<_, ResizeArray<_>>>()
                let newDict2 = new Dictionary<_, ResizeArray<_>>()
                let newArr = new ResizeArray<_>()
                newArr.Add(e.Level)
                newDict2.Add(e.NontermLabel, newArr)
                newDict1.Add(ast, newDict2)
                false, Some newDict1   

    let finalMatching (curRight : INode) nontermName finalExtensions findSppfNode findSppfPackedNode currentGSSNode currentVertexInInput (pop : Vertex -> int -> int<nodeMeasure> -> unit)  = 
        match curRight with
        | :? TerminalNode as t ->
            currentN := getNodeP findSppfNode findSppfPackedNode dummy !currentLabel !currentR !currentN
            let r = (sppfNodes.Item (int !currentN)) :?> NonTerminalNode 
            pop !currentGSSNode !currentVertexInInput !currentN
        | :? NonTerminalNode as r ->
            if (r.Name = nontermName) && (Array.exists ((=) r.Extension) finalExtensions)
            then 
                match !resultAST with
                | None ->  resultAST := Some r
                | Some a -> a.AddChild r.First
                         
            pop !currentGSSNode !currentVertexInInput !currentN
        | x -> failwithf "Unexpected node type in ASTGLL: %s" <| x.GetType().ToString()
          
                 
             
    member this.GetNodeP = getNodeP
    member this.SetP = setP
    member this.EpsilonNode = epsilonNode
    member this.SetR = setR
    member this.SppfNodes = sppfNodes
    member this.DummyAST = dummyAST
    //member this.PushContext = pushContext
    member this.ContainsContext = containsContext
    member this.AddContext = addContext
    member this.ContainsEdge = containsEdge
    member this.GetTreeExtension = getTreeExtension
    member this.Dummy = dummy
    member this.CurrentN = currentN
    member this.CurrentR = currentR 
    member this.ResultAST = resultAST
    member this.CurrentLabel = currentLabel
    member this.FinalMatching = finalMatching