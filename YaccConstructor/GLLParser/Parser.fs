
module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic 
//open Yard.Generators.RNGLR.DataStructures
open Yard.Generators.GLL
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Generators.RNGLR.DataStructures

//descriptor

type Label =  
    val Rule             : int
    val mutable Position : int 
    static member  Equal (label1 : Label) (label2 : Label) =
        let mutable result = true
        if label1.Rule <> label2.Rule || label1.Position <> label2.Position then result <- false
        result 
    new (rule : int,position : int) = {Rule = rule; Position = position} 

type IntermidiateNode =
    val LeftChild  : obj
    val RightChild : obj
    val Pos        : int * int
    new (l , r, p) = {LeftChild = l; RightChild = r; Pos = p}

[<AllowNullLiteral>]
type Vertex  =
    val mutable OutEdges : UsualOne<Edge>
    val Level            : int
    val Value            : Label
    static member Equal (v1 : Vertex) (v2 : Vertex) =
        let mutable res = false
        if v1.Level = v2.Level && Label.Equal v1.Value v2.Value then
            res <- true
        res
    new (value, level) = {OutEdges = Unchecked.defaultof<_>; Value = value; Level = level}

and Edge =
    struct
        val Ast   : obj
        val Dest  : Vertex
        new (d, a) = {Dest = d; Ast = a}
    end

/// Compare vertex like a pair: (level, state)
let inline private less (v' : Vertex) (v : Vertex) = v'.Level < v.Level 
let inline private eq (v' : Vertex) (v : Vertex) = v'.Level = v.Level && Label.Equal v'.Value v.Value



type Context =
    val Index         : int
    val Label         : Label
    val Node          : Vertex
    val Ast           : obj
    member this.Equal(indx, lbl, vrtx, sppf) =
        let mutable res = false
        if this.Index = indx && Label.Equal this.Label lbl && eq this.Node vrtx ///как сравнивать ячейки sppf? by reference
            then res <- true
        res    
    new (index, label, node, ast) = {Index = index; Label = label; Node = node; Ast = ast}


type ParseResult<'s> =
    | Success of 's
    | Error of 's

let containsContext (set : IEnumerable<Context>) (index : int) (label : Label) (gssNode : Vertex) (sppfNode : Family) =
    let mutable res = false
    for cntxt in set do
        if cntxt.Equal(index, label, gssNode, sppfNode) then
            res <- true
    res

(*Delete intermidiate nodes and return new family*)
let handleIntermidiate (node : IntermidiateNode) (prod : int) = 
    let rec handle (o : obj) =
        let result =
            match o with
            | :? IntermidiateNode as interNode ->
                let t : IntermidiateNode = unbox interNode
                let fst = handle t.LeftChild
                let snd = handle t.RightChild
                List.append fst snd
            | :? Nodes as node -> [box <| node]
            | _ -> failwith "Unexpected type."
        result
    let res = List.toArray <| handle node
    new Family(prod, new Nodes(res))

let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let enum = tokens.GetEnumerator()
    let inputLength = Seq.length tokens
    let startNonTerm = parser.LeftSide.[parser.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
         fun i -> epsilons.[i]
    if not <| enum.MoveNext() || parser.IndexEOF = parser.TokenToNumber enum.Current then
        if parser.AcceptEmptyInput then
            Success ("UIIII")
        else
            Error ("UAAAA")     
    else
        let tokens = Seq.toArray tokens

        let setR = new Queue<Context>();   
        let setP = new Queue<Vertex * obj>();    
        let setU = Array.init inputLength (fun _ -> new List<Label * Vertex>())  
            
        
        let currentIndex = ref 0
        let previousIndex = ref 0

        let currentRule = parser.StartRule
        let dummy = box <| null
        let dummyGSSNode = new Vertex(new Label(currentRule, -1), !currentIndex)
        let currentLabel = ref <| new Label(currentRule, 0)
        let startLabel = new Label(currentRule, 0)
        let startGSSNode = new Vertex(!currentLabel, !currentIndex)
        
        let currentN = ref <| null
        let currentR = ref <| null

        let currentGSSNode = ref <| new Vertex(!currentLabel, !currentIndex)
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode, dummy)
        
        
        let gss = Array.init inputLength (fun _ -> new ResizeArray<Vertex>())

        let ast = Array.init parser.NonTermCount (fun _ -> new ResizeArray<obj>())

        let chainCanInferEpsilon rule pos =
            let curRule = parser.Rules.[rule]
            let mutable result = true
            for i = pos to curRule.Length - 1 do
                if result && not parser.canInferEpsilon.[curRule.[i]]
                then    
                    result <- false
            result
    //do we need AST&
        let containsContext (set : List<Label * Vertex>) (index : int) (label : Label) (gssNode) =
            let mutable result = false
            for cntxt in set do
                if not result && (fst cntxt).Equals label && (snd cntxt).Equals gssNode
                then result <- true 
            result

        let addContext (label : Label)  (index : int) (node : Vertex) (ast : obj) =
            let res =
                if not (containsContext setU.[index] index label node) 
                then
                    Some <| new Context(index, label, node, ast)
                else None
            res

        let getNodeP (label : Label) (left : obj) (right : obj) =
            let mutable result =  right
            let previousSym = parser.Rules.[label.Rule].[label.Position - 1]
            if parser.Rules.[label.Rule].Length >= label.Position + 1 
            then
                let nextSym = parser.Rules.[label.Rule].[label.Position]
                let cond = false
                if cond
                //parser.NumIsTerminal previousSym || not (chainCanInferEpsilon label.Rule (label.Position - 1)) && not (chainCanInferEpsilon label.Rule (label.Position))
                then 
                    box <| result

                else
                    if chainCanInferEpsilon label.Rule (label.Position)
                        then
                            label.Position <- parser.Rules.[label.Rule].Length
                    if not (left.Equals dummy) then
                        result <- new IntermidiateNode(left, right, (label.Rule, label.Position)) 
                    else result <- new IntermidiateNode(null, right, (label.Rule, label.Position))
                        
                    box <| result
            else 
                if not (left.Equals dummy) 
                then
                    result <- new IntermidiateNode(left, right, (label.Rule, label.Position)) 
                    box <| result
                else 
                    result <- new IntermidiateNode(null, right, (label.Rule, label.Position))
                    box <| result
            

//Create node for terminal.
        let getNodeT term b =
            box <| new Nodes(term, b, null)

        let containsGSSNode (l : Label) (i : int) =  
            let res = gss.[i].Find (fun elem -> l.Equals elem)
            if res <> null
            then
                res
            else
                new Vertex(l, i)
        //проверить AST
        let containsEdge (b : Vertex) (e : Vertex) (ast : obj)=
            let edges = b.OutEdges
            let mutable result = false
            if edges.first <> Unchecked.defaultof<_>
            then
                if not (eq edges.first.Dest e)
                then
                    if edges.other <> null
                    then
                        for edge in edges.other do
                            if eq edge.Dest e
                            then
                                result <- true
                else result <- true
            result
                           

        let create (label : Label) (u : Vertex) (index : int) (ast : obj) = 
            let v = containsGSSNode label index
            if not (containsEdge v u ast)
            then
                let newEdge = new Edge(u,ast)
                for pair in setP do
                    if eq v (fst pair) 
                    then 
                        let y = getNodeP label ast (snd pair)
                        let cntxt = addContext label index u y
                        if  cntxt.IsSome
                        then
                            setU.[index].Add (!currentLabel, !currentGSSNode)
                            setR.Enqueue(cntxt.Value)    
                if v.OutEdges.first <> Unchecked.defaultof<_>
                then
                    v.OutEdges.other <- Array.append v.OutEdges.other [| newEdge |]
                else v.OutEdges.first <- newEdge
            gss.[index].Add v
            v
          

(*For each edge (cU , z, u) in the GSS, where cU denotes the current stack top, getNodeP() is called to construct an
intermediate or symbol node, w, with a packed node whose left child is the node z retrieved from the GSS edge between cU
and u and whose right child is the SPPF node corresponding to xq+1. The function getNodeT () simply constructs a terminal
labelled SPPF node. A descriptor of the form (Rxq+1 , u, cI ,w) is then created. When such a descriptor is removed from R
execution continues with the input pointer at position cI , the grammar pointer after xq+1, and cU := u and cN := w.*)
        let pop (u : Vertex) (i : int) (z : obj) =
            if not (eq u startGSSNode) then
                let label = u.Value
                setP.Enqueue(u, z)
                let processEdge (edge : Edge) =
                    let y = getNodeP label edge.Ast z 
                    let cntxt = addContext label i edge.Dest y
                    if  cntxt.IsSome
                    then
                        setU.[i].Add (!currentLabel, !currentGSSNode)
                        setR.Enqueue(cntxt.Value)    
                processEdge u.OutEdges.first
                for edge in u.OutEdges.other do
                    processEdge edge

        let table = parser.Table
   
        let  condition = ref true  

/////Control////   
        let rec dispatcher () =  
         
            if setR.Count <> 0 then
                currentContext := setR.Dequeue()
                currentIndex := currentContext.Value.Index
                if !currentIndex <> !previousIndex 
                then
                    //setU.[!previousIndex].Clear()
                    previousIndex := !currentIndex 
                currentGSSNode := currentContext.Value.Node
                currentLabel := currentContext.Value.Label
                currentN := currentContext.Value.Ast 
                processing()
            else Error ("fail") 
                       
        and processing () =  

            let getIndex(nTerm, term) = 
                let mutable index = nTerm
                index <- (index * (parser.IndexatorFullCount - parser.NonTermCount))
                index <- index + term - parser.NonTermCount
                index

            
            
(*
Если правило ещё не закончилось, то нужно понять, кого обрабатываем.
*)
            if Array.length parser.Rules.[currentLabel.Value.Rule]  <> currentLabel.Value.Position then
                let curToken = parser.TokenToNumber tokens.[!currentIndex]
                let curSymbol = parser.Rules.[currentLabel.Value.Rule].[currentLabel.Value.Position]
                if parser.NumIsTerminal curSymbol  || parser.NumIsLiteral curSymbol then
                    if curSymbol = curToken
                    then
                        if !currentN = null 
                        then
                            currentN := getNodeT curSymbol !currentIndex
                        else
                            currentR := getNodeT curSymbol !currentIndex
                        currentIndex := !currentIndex + 1
                        currentLabel.Value.Position <- currentLabel.Value.Position + 1
                        if !currentR <> null
                        then
                            currentN := getNodeP !currentLabel !currentN !currentR
                        condition := false

                elif parser.NumIsNonTerminal curSymbol then 
                    let index = getIndex(curSymbol, curToken)
                    let temp = table.[index]
                    if Array.length table.[index] <> 0 then
                        currentGSSNode := create (new Label(currentLabel.Value.Rule, currentLabel.Value.Position + 1)) !currentGSSNode !currentIndex currentN
                        for ruleN in table.[index] do
                            let newLabel = new Label(ruleN, 0)
                            let cntxt = addContext newLabel !currentIndex !currentGSSNode dummy
                            if  cntxt.IsSome
                            then
                                setU.[!currentIndex].Add (!currentLabel, !currentGSSNode)
                                setR.Enqueue(cntxt.Value)    
                            
                    condition := true

            else 
                //currentN := getNodeP !currentLabel !currentN !currentR 
                let curRight : IntermidiateNode = unbox <| !currentN
                let resTree = handleIntermidiate curRight currentLabel.Value.Rule
                ast.[parser.LeftSide.[currentLabel.Value.Rule]].Add resTree
                pop !currentGSSNode !currentIndex resTree
                condition := true
            if !condition then dispatcher() else processing()
        processing()