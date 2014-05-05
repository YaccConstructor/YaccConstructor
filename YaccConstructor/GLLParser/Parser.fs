
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
[<Struct>]
type Label = 
    val Rule     : int
    val Position : int
    static member  Equal (label1 : Label) (label2 : Label) =
        let mutable result = true
        if label1.Rule <> label2.Rule || label1.Position <> label2.Position then result <- false
        result 
    new (rule : int,position : int) = {Rule = rule; Position = position} 

type GSSNode = 
    val Index  : int   
    val Value  : Label 
    val mutable OutEdges : UsualOne<GSSEdge>
    static member  Equal (node1 : GSSNode) (node2 : GSSNode) =
        let mutable result = true
        if node1.Index <> node2.Index || not(Label.Equal node1.Value node2.Value) then result <- false
        result  
    new (index, value) = {OutEdges = Unchecked.defaultof<_>; Index = index; Value = value}

and GSSEdge =
    val Dst    : GSSNode
     /// AST on the edge
    val Ast    : obj
    member  this.Equal (nodeDst : GSSNode) =
        let mutable result = true
        if not (GSSNode.Equal this.Dst nodeDst) then result <- false
        result  
    new (src, dst, ast) = {Dst = dst; Ast = ast}
//type GSS (len) =
//    let length = len
//    let mutable edgeSet = new List<GSSEdge>() 
//    member this.AddEdge (newEdge : GSSEdge)=
//        edgeSet.Add newEdge
//    member this.Length = length
//    member this.EdgeSet = edgeSet

type Context =
    val Index    : int  //index in the input stream
    val Label    : Label //current label
    val Node     : GSSNode //current stack
    val Ast     : obj
    static member  Equal (con1:Context) (con2:Context) =
        let mutable result = true
        if con1.Index <> con2.Index || con1.Label <> con2.Label || not (GSSNode.Equal con1.Node con2.Node) then result <- false
        result 
    new (index, label, node, ast) = {Index = index; Label = label; Node = node; Ast = ast}

type ParseResult<'s> =
    | Success of 's
    | Error of 's

let containsContext (set : Queue<Context>) (context : Context) =
    let mutable result = false
    for curCon in set do
        if Context.Equal curCon context 
        then 
            result <- true
    result
             
let containsContextParams (set:IEnumerable<Context>) label node index = 
    let mutable result = false
    for curCon in set do
        if curCon.Index = index && Label.Equal curCon.Label label && GSSNode.Equal curCon.Node node 
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


type Incrementor(delta) =
    member this.Increment(i : int byref) =
        i <- i + delta
    member this.Decrement(i : int byref) =
        i <- i - delta
    

let buildAst<'TokenType> (parser : ParserSource2<'TokenType>) (tokens : seq<'TokenType>) : ParseResult<_> = 
    let enum = tokens.GetEnumerator()
    let inputLength = Seq.length tokens
    let startNonTerm = parser.LeftSide.[parser.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parser.LeftSide)
    let getEpsilon =

         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
         fun i -> epsilons.[i]
    // Currently processed token
   // let curToken = ref enum.Current
    //let curNum = ref (parser .TokenToNumber enum.Current)
    // If input stream is empty or consists only of _EOF token
    if not <| enum.MoveNext() || parser.IndexEOF = parser.TokenToNumber enum.Current then
        if parser.AcceptEmptyInput then
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
        let currentIndex = ref 0
        let currentN = new Nodes()
        let currentR = new Nodes()
        let currentRule = parser.StartRule
        let dummyNode = new GSSNode(!currentIndex, new Label(currentRule, -1))
        let currentLabel = ref <| new Label(currentRule, 0);
        let startLabel = new Label(currentRule, 0);
        let startGSSNode = new GSSNode(!currentIndex,!currentLabel);
        let currentGSSNode = ref <| new GSSNode(!currentIndex,!currentLabel);
        let gss =  new GSS(dummyNode, tokens.Length)
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode)
        let mutable firstTime = true
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
                    if GSSNode.Equal edge.Src node then
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
                currentLabel := currentContext.Value.Label
                processing()
            elif containsContextParams setU.[inputLength-1] startLabel  startGSSNode inputLength then 
                Success ("UIIII") else Error ("fail")        
        and processing () = 
            let curInd = !currentIndex 
            let curToken = parser.TokenToNumber tokens.[curInd]
            let curSymbol = parser.Rules.[currentLabel.Value.Rule].[currentLabel.Value.Position]
            if Array.length parser.Rules.[currentLabel.Value.Rule]  <> currentLabel.Value.Position then
                if parser.NumIsTerminal curSymbol  || parser.IsLiteral tokens.[curInd] then
                    if curToken = curSymbol then
                        currentIndex := !currentIndex+1
                        pop !currentContext !currentGSSNode !currentIndex
                elif parser.NumIsNonTerminal parser.Rules.[currentLabel.Value.Rule].[currentLabel.Value.Position] then 
                    let mutable index = currentLabel.Value.Rule
                    index <- (index*(parser.IndexatorFullCount))
                    index <- index + currentLabel.Value.Position
                    if Array.length parser.Table.[index] <> 0 then
                        let position = 0
                        for rulesN in parser.Table.[index] do
                            //addContextRulePosition rulesN position !currentGSSNode !currentIndex
                            updateGSS !currentLabel !currentIndex !currentGSSNode
            else 
                pop !currentContext !currentGSSNode !currentIndex
            dispatcher ()
        processing()
//        match dispatcher() with
//        | Error _ -> Error ("fail")
//        | Success _ -> Success "UIIII"
