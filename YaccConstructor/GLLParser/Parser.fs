
module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open System 
open System.Collections.Generic 
//open Yard.Generators.RNGLR.DataStructures
open Yard.Generators.GLL
open Yard.Generators.RNGLR

//descriptor
[<Struct>]
type Label = 
    val Rule     : int ref
    val Position : int ref
    static member  Equal (label1 : Label) (label2 : Label) =
        let mutable result = true
        if label1.Rule <> label2.Rule || label1.Position <> label2.Position then result <- false
        result 
    new (rule : int,position : int) = {Rule = ref rule; Position = ref position} 

//[<Struct>]
//type Label = 
//    val mutable Rule     : int
//    val mutable Position : int
//    static member  Equal (label1 : Label) (label2 : Label) =
//        let mutable result = true
//        if label1.Rule <> label2.Rule || label1.Position <> label2.Position then result <- false
//        result 
//    new (rule ,position) = {Rule = rule; Position = position} 


type GSSNode = 
    val Index : int   
    val Value  : Label 
    static member  Equal (node1 : GSSNode) (node2 : GSSNode) =
        let mutable result = true
        if node1.Index <> node2.Index || not(Label.Equal node1.Value node2.Value) then result <- false
        result  
    new (index, value) = {Index = index; Value = value}

type GSSEdge =
    val Src      : GSSNode
    val Dst      : GSSNode
    member  this.Equal (nodeSrc : GSSNode) (nodeDst : GSSNode) =
        let mutable result = true
        if not (GSSNode.Equal this.Src nodeSrc) || not (GSSNode.Equal this.Dst nodeDst) then result <- false
        result  
    new (src, dst) = {Src = src; Dst = dst}

type GSS (node, len) =
    let dummyNode = node
    let length = len
    let mutable edgeSet = new List<GSSEdge>() 
    member this.AddEdge (newEdge : GSSEdge)=
        edgeSet.Add newEdge
    member this.Dummy = dummyNode
    member this.Length = length
    member this.EdgeSet = edgeSet

type Context =
    val Index    : int
    val LabelC   : Label
    val Node     : GSSNode
    static member  Equal (con1:Context) (con2:Context) =
        let mutable result = true
        if con1.Index <> con2.Index || con1.LabelC <> con2.LabelC || not (GSSNode.Equal con1.Node con2.Node) then result <- false
        result 
    new (index, label, node) = {Index = index; LabelC = label; Node = node}

type ParseResult<'s> =
    | Success of 's
    | Error of 's

let containsContext (set:Queue<Context>) (context:Context) =
    let mutable result = false
    for curCon in set do
        if Context.Equal curCon context 
        then 
            result <- true
    result
             
let containsContextParams (set:IEnumerable<Context>) label node index = 
    let mutable result = false
    for curCon in set do
        if curCon.Index = index && Label.Equal curCon.LabelC label && GSSNode.Equal curCon.Node node 
        then 
            result <- true
    result

let gssContainsNode (gss : GSS) (label : Label) (index : int) = 
    let result = ref None
    for curEdge in gss.EdgeSet do    
         if Label.Equal curEdge.Dst.Value label && curEdge.Dst.Index = index then
             result := Some(curEdge.Dst) 
         elif Label.Equal curEdge.Src.Value label && curEdge.Src.Index = index then
            result := Some(curEdge.Src) 
    !result

let gssContainsEdge (gss : GSS) (nodeSrc : GSSNode) (nodeDst : GSSNode) = 
    let mutable result = false
    for edges in gss.EdgeSet do
        if edges.Equal nodeSrc nodeDst then
            result <- true
    result

(*let next =
    let i = ref -1
    fun () -> 
        //let j = !i
        incr i
        !i
*)

type Incrementor(delta) =
    member this.Increment(i : int byref) =
        i <- i + delta
    member this.Decrement(i : int byref) =
        i <- i - delta
    

let buildAst<'TokenType> (parserSourceGLL : ParserSourceGLL<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let enum = tokens.GetEnumerator()
    let inputLength = Seq.length tokens
    let startNonTerm = parserSourceGLL.LeftSide.[parserSourceGLL.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parserSourceGLL.LeftSide)
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
         fun i -> epsilons.[i]
    // Currently processed token
   // let curToken = ref enum.Current
    //let curNum = ref (parserSourceGLL.TokenToNumber enum.Current)
    // If input stream is empty or consists only of _EOF token
    if not <| enum.MoveNext() || parserSourceGLL.IndexEOF = parserSourceGLL.TokenToNumber enum.Current then
        if parserSourceGLL.AcceptEmptyInput then
            Success ("UIIII")
        else
            Error ("UAAAA")     
    else
        let incrementor = new Incrementor(1)
// sets for correct work of algorithm
        let tokens = Seq.toArray tokens
        let setU = Array.create tokens.Length List.empty<Context>
        let setR = new Queue<Context>();   // множество всех контекстов
        let setP = new Queue<GSSNode>();   //множество для потенциально незавершаемых попов 
//initialize different necessary values       
        let currentIndex = ref 0
        let currentLabel = ref <| new Label(0, 0);
        let startLabel = new Label(0, 0);
        let currentGSSNodeName = ref 0
        let startGSSNode = new GSSNode(!currentGSSNodeName,!currentLabel);
        let currentGSSNode = ref <| new GSSNode(!currentGSSNodeName,!currentLabel);
        let gss =  new GSS(startGSSNode, tokens.Length)
        let currentContextName = ref 0
        let currentContext = ref <| new Context(!currentContextName,!currentLabel,!currentGSSNode)

        setR.Enqueue(!currentContext)
        
        
        let addContext (label:Label) (node:GSSNode) (index:int) =
            if not (containsContextParams setU.[index] label node index) 
            then
                setU.[index] <- new Context(index, label, node) :: setU.[index]
                setR.Enqueue (new Context(index, label, node))

        let addContextRulePosition (rule : int) (position : int) (node:GSSNode) (index:int) =
            let label = new Label (rule, position)
            if not (containsContextParams setU.[index] label node index) 
            then
                setU.[index] <- new Context(index, label, node) :: setU.[index]
                setR.Enqueue (new Context(index, label, node))

        let pop (context:Context) (node: GSSNode) (index:int) =
            if not (GSSNode.Equal node startGSSNode) 
            then
                setP.Enqueue node
                for edge in gss.EdgeSet do
                    if GSSNode.Equal edge.Src node 
                    then
                        addContext node.Value edge.Dst index  
        
        let updateGSS (label:Label) (index : int) (node:GSSNode) =
            let smth = gssContainsNode gss label index
            let newNode = 
                match smth with
                |Some _ -> smth.Value
                |None -> new GSSNode(index, label)
            if not (gssContainsEdge gss newNode node) then
                gss.AddEdge (new GSSEdge (newNode, node)) 
        
//        Success ("Test")        
        let rec dispatcher () =  
            if setR.Count <> 0 then
                currentContext := setR.Dequeue()
                currentIndex := currentContext.Value.Index
                currentGSSNode := currentContext.Value.Node
                currentLabel := currentContext.Value.LabelC
                processing()
            elif containsContextParams setU.[inputLength] startLabel  startGSSNode inputLength then 
                Success ("UIIII") else Error ("fail")        
        and processing () = 
            let curInd = currentIndex
            if Array.length parserSourceGLL.Rules.[!currentLabel.Value.Rule]  <> !currentLabel.Value.Position then
                if parserSourceGLL.NumIsTerminal parserSourceGLL.Rules.[!currentLabel.Value.Rule].[!currentLabel.Value.Position] then 
                   incrementor.Increment(currentLabel.Value.Position)
                   currentIndex := !currentIndex+1
                elif parserSourceGLL.NumIsNonTerminal parserSourceGLL.Rules.[!currentLabel.Value.Rule].[!currentLabel.Value.Position] then 
                    incrementor.Decrement currentLabel.Value.Rule
                    let mutable index = !currentLabel.Value.Rule
                    index <- (index*(parserSourceGLL.IndexatorFullCount))
                    index <- index + !currentLabel.Value.Position
                    if Array.length parserSourceGLL.Table.[index] <> 0 then
                        incrementor.Decrement currentLabel.Value.Rule
                        let mutable index= !currentLabel.Value.Rule
                        index <- index * parserSourceGLL.IndexatorFullCount
                        index <- index + !currentLabel.Value.Position
                        for rulesN in parserSourceGLL.Table.[index] do
                            addContextRulePosition rulesN 0 !currentGSSNode !currentIndex
            else 
                pop !currentContext !currentGSSNode !currentIndex
            dispatcher ()
        dispatcher()
//        match dispatcher() with
//        | Error _ -> Error ("fail")
//        | Success _ -> Success "UIIII"
