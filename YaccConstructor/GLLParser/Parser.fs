
module Yard.Generators.GLL.Parser 
open Yard.Generators.GLL 
open Yard.Generators.RNGLR
open System 
open System.Collections.Generic 
open Yard.Generators.RNGLR.DataStructures
open  Yard.Generators.GLL

//descriptor
[<Struct>]
type Label = 
    val mutable Rule     : int
    val mutable Position : int
    static member  Equal (label1 : Label) (label2 : Label) =
        let mutable result = true
        if label1.Rule <> label2.Rule || label1.Position <> label2.Position then result <- false
        result 
    new (rule,position) = {Rule = rule; Position = position} 

type GSSNode = 
    val LabelN : int   //имя ячейки
    val Value  : Label 
    static member  Equal (node1 : GSSNode) (node2 : GSSNode) =
        let mutable result = true
        if node1.LabelN <> node2.LabelN || not(Label.Equal node1.Value node2.Value) then result <- false
        result  
    new (labelN, value) = {LabelN = labelN; Value = value}

type GSSEdge =
    val Src      : GSSNode
    val Dst      : GSSNode
    new (src, dst) = {Src = src; Dst = dst}

type GSS =
    val EdgeSet  : List<GSSEdge>
    member this.addNode newNode = this.EdgeSet.Add newNode
    new () = {EdgeSet = new List<GSSEdge>()}

type Context =
    val Index    : int
    val LabelC   : Label
    val Node     : GSSNode
    static member  Equal (con1:Context) (con2:Context) =
        let mutable result = true
        if con1.Index <> con2.Index || con1.LabelC <> con2.LabelC || not (GSSNode.Equal con1.Node con2.Node) then result <- false
        result 
    new (index, label, node) = {Index = index; LabelC = label; Node = node}

type ParseResult<'string> =
    | Success of string
    | Error of string

let setR = new Queue<Context>();   // множество всех контекстов
let setU = new Queue<Context>();   //множество, чтобы не создавать один и тот же дескриптор дважды
let setP = new Queue<GSSNode>();   //множество для потенциально незавершаемых попов
let gss =  new GSS()
let mutable currentIndex = 0
let mutable currentRule = 0
let mutable currentPosition = 0
let mutable currentLabel = new Label(currentRule, currentPosition);
let startLabel = new Label(currentRule, currentPosition);
let mutable currentGSSNodeName = 0
let startGSSNode = new GSSNode(currentGSSNodeName,currentLabel);
let mutable currentGSSNode = new GSSNode(currentGSSNodeName,currentLabel);
let gss = new GSS()
let startRuleNum = 0   //заглушка
let lengthOfString = 100 //заглушка 
let mutable currentContextName = 0
let mutable currentContext = new Context(currentContextName,currentLabel,currentGSSNode)
setR.Enqueue(currentContext)

let containsContext (set:Queue<Context>) (context:Context) =
    let mutable result = false
    for curCon in set do
        if Context.Equal curCon context 
        then 
            result <- true
    result

let containsContextParams (set:Queue<Context>) index label node = 
    let mutable result = false
    for curCon in set do
        if curCon.Index = index && Label.Equal curCon.LabelC label && GSSNode.Equal curCon.Node node 
        then 
            result <- true
    result

let addContext (node:GSSNode) =
    for cntxt in setU do
        if GSSNode.Equal cntxt.Node node && cntxt.Index = currentIndex then
            setU.Enqueue (new Context(currentContextName,currentLabel,node))

let pop () =
    if GSSNode.Equal currentGSSNode startGSSNode 
    then setP.Enqueue currentGSSNode
    for node in gss.EdgeSet do
        if GSSNode.Equal node.Dst currentGSSNode then
            addContext node.Dst

let updateGSS () = 
    printf "updateGSS"

let buildAst<'TokenType> (parserSource : ParserSource<'TokenType>) (tokens : seq<'TokenType>) = 
    let enum = tokens.GetEnumerator()
    let inputLength = Seq.length tokens
    let startNonTerm = parserSource.LeftSide.[parserSource.StartRule]
    let nonTermsCountLimit = 1 + (Array.max parserSource.LeftSide)
    let getEpsilon =
         let epsilons = Array.init nonTermsCountLimit (fun i -> box (-i-1))
         fun i -> epsilons.[i]
    // Currently processed token
    let curToken = ref enum.Current
    let curNum = ref (parserSource.TokenToNumber enum.Current)
    // If input stream is empty or consists only of _EOF token
    if not <| enum.MoveNext() || parserSource.IndexEOF = parserSource.TokenToNumber enum.Current then
        if parserSource.AcceptEmptyInput 
        then
            Success ("UIIII")
        else
            Success ("UIIII")
           
    else  
        let tokens = Seq.toArray tokens                                   
        let rec dispatcher () : ParseResult<'string> =     
            if setR.Count <> 0 then
                let currentContext : Context = setR.Dequeue()
                currentIndex <- currentContext.Index
                currentGSSNode <- currentContext.Node
                currentLabel <- currentContext.LabelC
                processing
            elif containsContextParams setU inputLength startLabel startGSSNode then 
                Success ("UIIII") else Fail ("fail")
        and processing () : ParseResult<'string> = 
            if parserSource.Rules.[currentLabel.Rule] |> Array.length <> currentLabel.Position then
                if parserSource.Rules.[currentLabel.Rule].[currentLabel.Position] |> parserSource.IsTerminal then 
                   currentLabel.Position <- currentLabel.Position + 1
                   currentIndex <- currentIndex + 1
                   dispatcher 
                elif parserSource.Rules.[currentLabel.Rule].[currentLabel.Position] |> parserSource.IsNonTerminal 
                then dispatcher 
            else 
                pop
                dispatcher
    printf "ffdf"

    

         
                
