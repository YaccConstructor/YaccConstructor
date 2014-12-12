module Yard.Generators.Common.AST3
open System
open System.Collections.Generic
open Yard.Generators.Common.DataStructures

[<Measure>] type extension


[<AllowNullLiteral>]
type INode = 
    interface
    end

[<AllowNullLiteral>]
type NonTerminalNode =
    interface INode
    val Name      : int
    val Extension : int64<extension>
    val mutable First     : int 
    val mutable Others    : ResizeArray<int> 
    member this.AddChild child : unit = 
        if this.First <> Unchecked.defaultof<_>
        then 
            if this.Others <> Unchecked.defaultof<_>
            then
                this.Others.Add child
            else
                this.Others <- new ResizeArray<int>()
                this.Others.Add child
        else this.First <- child
    new (name, extension) = {Name = name; Extension = extension; First = Unchecked.defaultof<_>; Others = Unchecked.defaultof<_>}
    
and TerminalNode =
    interface INode
    val Name : int
    val Extension : int<extension>
    new (name, extension) = {Name = name; Extension = extension}

and PackedNode =
    interface INode    
    val Production : int
    val Fst : INode
    val Snd : INode
    new (p, f, s) = {Production = p; Fst = f; Snd = s}

and IntermidiateNode = 
    interface INode
    val Slot      : int
    val Extension : int<extension>
    val Left      : INode
    val Right     : INode
    new (slot, extension, left, right) = {Slot = slot; Extension = extension; Left = left; Right = right}
    

type private DotNodeType = Packed | NonTerminal | Intermidiate | Terminal

let inline packExtension left right : int64<extension> =  LanguagePrimitives.Int64WithMeasure ((int64 left <<< 32) ||| int64 right)
let inline getRightExtension (long : int64<extension>) = int <| ((int64 long) &&& 0xffffffffL)
let inline getLeftExtension (long : int64<extension>)  = int <| ((int64 long) >>> 32)

let inline packLabel rule position = (int rule <<< 16) ||| int position
let inline getRule packedValue = int packedValue >>> 16
let inline getPosition (packedValue : int) = int (packedValue &&& 0xffff)

[<Struct>]
type NumNode =
    val Num : int
    val Node : obj
    new (num, node) = {Num = num; Node = node} 

[<AllowNullLiteral>]
type Tree<'TokenType> (tokens : array<'TokenType>, root : obj, rules : int[][]) =
   
    member this.AstToDot (indToString : int -> string) tokenToNumber (leftSide : array<int>) (path : string) =
        use out = new System.IO.StreamWriter (path : string)
        out.WriteLine("digraph AST {")

        let createNode num isAmbiguous nodeType (str : string) =
            let label =
                let cur = str.Replace("\n", "\\n").Replace ("\r", "")
                if not isAmbiguous then cur
                else cur + " !"
            let shape =
                match nodeType with
                | Intermidiate -> ",shape=box"
                | Packed -> ",shape=circle"
                | Terminal -> ",shape=box"
                | NonTerminal -> ",shape=oval"
            let color =
                if not isAmbiguous then ""
                else ",style=\"filled\",fillcolor=red"
            out.WriteLine ("    " + num.ToString() + " [label=\"" + label + "\"" + color + shape + "]")

        let createEdge (b : int) (e : int) isBold (str : string) =
            let label = str.Replace("\n", "\\n").Replace ("\r", "")
            let bold = 
                if not isBold then ""
                else "style=bold,width=10,"
            out.WriteLine ("    " + b.ToString() + " -> " + e.ToString() + " [" + bold + "label=\"" + label + "\"" + "]")
        
        let nodeQueue = new Queue<NumNode>()
        let used = new Dictionary<_,_>()
        let num = ref -1
        nodeQueue.Enqueue(new NumNode(!num, root))
        while nodeQueue.Count <> 0 do
            let currentPair = nodeQueue.Dequeue()
            let key = ref 0
            if !num <> -1
            then

                if currentPair.Node <> null && used.TryGetValue(currentPair.Node, key)
                then
                    createEdge currentPair.Num !key false ""
                else    
                    num := !num + 1
                    used.Add(currentPair.Node, !num)
                    match currentPair.Node with 
                    | :? NonTerminalNode as a -> 
                        createNode !num false NonTerminal (indToString leftSide.[a.Name])
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, a.First))
                        if a.Others <> Unchecked.defaultof<_>
                        then
                            for n in a.Others do
                                nodeQueue.Enqueue(new NumNode(!num, n))
                    | :? PackedNode as p ->
                        createNode !num false Packed ""
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, p.Fst))
                        if p.Snd <> null
                        then nodeQueue.Enqueue(new NumNode(!num, p.Snd))
                    | :? IntermidiateNode as i ->
                        createNode !num false Intermidiate ((getRule i.Slot).ToString() + " " + (getPosition i.Slot).ToString())
                        createEdge currentPair.Num !num false ""
                        nodeQueue.Enqueue(new NumNode(!num, i.Left))
                        nodeQueue.Enqueue(new NumNode(!num, i.Right))
                    | :? TerminalNode as t ->
                        createNode !num false Terminal ("t " + indToString (tokenToNumber tokens.[t.Name]))
                        createEdge currentPair.Num !num false ""
                    | null -> ()
            else
                let a = currentPair.Node :?> NonTerminalNode
                num := !num + 1
                createNode !num false NonTerminal (indToString leftSide.[a.Name])
                nodeQueue.Enqueue(new NumNode(!num, a.First))
                if a.Others <> Unchecked.defaultof<_>
                then
                    for n in a.Others do
                        nodeQueue.Enqueue(new NumNode(!num, n))


        out.WriteLine("}")
        out.Close()
