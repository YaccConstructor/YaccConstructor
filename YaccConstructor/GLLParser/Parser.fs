
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
    val GssNode       : Vertex
    val SppfNode      : obj
    member this.Equal(indx, lbl, vrtx, sppf) =
        let mutable res = false
        if this.Index = indx && Label.Equal this.Label lbl && eq this.GssNode vrtx ///как сравнивать ячейки sppf
            then res <- true
        res    
    new (index, label, node, ast) = {Index = index; Label = label; GssNode = node; SppfNode = ast}


type ParseResult<'s> =
    | Success of 's
    | Error of 's

let containsContext (set : IEnumerable<Context>) (index : int) (label : Label) (gssNode : Vertex) (sppfNode : Family) =
    let mutable res = false
    for cntxt in set do
        if cntxt.Equal(index, label, gssNode, sppfNode) then
            res <- true
    res

    
let deleteIntermidiateNodes (v : Family) = 
    let rec handle (o : obj) =
        let res =
            match o with
            | :? AST as ast -> box <| ast
            | :? IntermidiateNode as interNode ->
                let t : IntermidiateNode = unbox interNode
                let fst = handle t.leftChild
                let snd = handle t.rightChild
                box <| new Nodes([|fst, snd|])
            | :? Nodes as node -> box <| node
            | _ -> failwith "Unexpected type."
        res
    let arr = v.nodes.map handle
    new Family(v.prod, new Nodes(arr))

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
        let allGSSNodes = Array.create tokens.Length List.empty<Vertex>
        let setR = new Queue<Context>();   // множество всех контекстов
        let setP = new Queue<Vertex * obj>();   //множество для потенциально незавершаемых попов       
        let currentIndex = ref 0
        let currentN = new Nodes()
        let currentR = new Nodes()
        let currentRule = parser.StartRule
        let dummy = new Nodes()
        let dummyGSSNode = new Vertex(new Label(currentRule, -1), !currentIndex)
        let currentLabel = ref <| new Label(currentRule, 0)
        let startLabel = new Label(currentRule, 0)
        let startGSSNode = new Vertex(!currentLabel, !currentIndex)
        let currentGSSNode = ref <| new Vertex(!currentLabel, !currentIndex)
        let currentContext = ref <| new Context(!currentIndex,!currentLabel,!currentGSSNode, dummy)
        setR.Enqueue(!currentContext)
        let sppfNodes = List.empty<obj>
        let intermidiateNodes = List.empty<IntermidiateNode>
        let terminalNodes = List.empty<Nodes>
        let familyNode = List.empty<Family>
        

        let chainCanInferEpsilon rule pos =
            let curRule = parser.Rules.[rule]
            let mutable result = true
            for i = pos to curRule.Length - 1 do
                if result && not parser.canInferEpsilon.[curRule.[i]]
                then    
                    result <- false
            result

        let containsGSSNode (label : Label) (index : int) = 
            let curList = allGSSNodes.[index]
            let mutable res = null 
            for vrtx in curList do
                if vrtx.Value.Equals label 
                then
                    res <- box <| vrtx
            res
        
        let addContext (label : Label)  (index : int) (gssNode : Vertex) (sppfNode : AST) =
            if not containsContext setU.[index] index label gssNode sppfNode then
                let cntxt = new Context(index,label, gssNode, sppfNode)
                setU.[index] <- cntxt :: setU.[index]
                setR.Enqueue(cntxt)

                //в результате хотим упаковать правую и левую и вернуть результат
        let getNodeP (label : Label) (left : obj) (right : Nodes) =
            let mutable result = box <| right
            let previousSym = parser.Rules.[label.Rule].[label.Position - 1]
            let nextSym = parser.Rules.[label.Rule].[label.Position + 1] /// check
            if parser.NumIsTerminal previousSym || not (chainCanInferEpsilon label.Rule (label.Position - 1)) && not (chainCanInferEpsilon label.Rule (label.Position + 1))
            then 
                result
            else
            //если может выводить эпсилон, то мы должны создать family со значением нетерминала в левой части текущего правила. 
                if chainCanInferEpsilon label.Rule (label.Position + 1)
                    then
                        label.Position <- parser.Rules.[label.Rule].Length
                if not (left.Equals dummy) then
                    result <- box <| new IntermidiateNode(left, right, (label.Rule, label.Position)) 
                else result <- box <| new IntermidiateNode(null, right, (label.Rule, label.Position))
                        
                result

                //Добавить проверку, что такой терминал не содержится
                //Для этого нужно понять, как хранить ячейки
                //Где и сколько
        let getNodeT term = 
            new Nodes([|term|])

        

        let create (label : Label) (gss : Vertex) (index : int) (ast : obj) = 
            let o = containsGSSNode label index
            let newNode =
                if o <> null 
                then
                    (unbox o) :> Vertex
                else new Vertex(label, index)
            let newEdge =
                let fst = newNode.OutEdges.first 
                if fst <> Unchecked.defaultof<Edge>
                then
                    if fst.Ast.Equals ast && Vertex.Equal fst.Dest gss 
                    then
                        fst
                    elif newNode.OutEdges.other <> null 
                    then
                        let mutable temp = None
                        for ed in newNode.OutEdges.other do
                            if temp.IsNone && ed.Ast.Equals ast && Vertex.Equal ed.Dest gss
                            then
                                temp <- Some <| ed
                        if temp.IsSome then
                            temp.Value
                        else new Edge(gss, ast)
                    else new Edge(gss, ast)
                else new Edge(gss, ast)
            newNode.OutEdges.other <- Array.append newNode.OutEdges.other [| newEdge |]

                                  


                        

                

                    

                

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
                    let y = getNodeP label, edge.Ast, z 
                    let unboxed : Nodes = unbox y
                    addContext label i edge.Dest unboxed
                processEdge u.OutEdges.first
                for edge in u.OutEdges.other do
                    processEdge edge


        




        let table = parser.Table
//        Success ("Test")   
        let mutable condition = true  
/////Main method////   
        let rec dispatcher () =   
            if setR.Count <> 0 then
                currentContext := setR.Dequeue()
                currentIndex := currentContext.Value.Index
                currentGSSNode := currentContext.Value.GssNode
                currentLabel := currentContext.Value.Label
                let mutable currentPosition = currentLabel.Value.Position
                processing()
            elif containsContextParams setU.[inputLength-1] startLabel  startGSSNode inputLength then ///////////////???????????
                Success ("UIIII") else Error ("fail")        
        and processing () =  
(*
Берем индекс для таблицы.
*)
            let getIndex() = 
                let mutable index = currentLabel.Value.Rule
                index <- (index*(parser.IndexatorFullCount))
                index <- index + currentLabel.Value.Position
                index

            let mutable currentPosition = currentLabel.Value.Position
            let curToken = parser.TokenToNumber tokens.[!currentIndex]
            let curSymbol = parser.Rules.[currentLabel.Value.Rule].[currentPosition]
            
(*
Если правило ещё не закончилось, то нужно понять, кого обрабатываем.
*)
            if Array.length parser.Rules.[currentLabel.Value.Rule]  <> currentLabel.Value.Position then
(*
Обрабатываем терминал. Нужно считать текущий символ, обновить правое дерево и сдвинуть позицию. 
После этого разбор продолжается.
*)
                if parser.NumIsTerminal curSymbol  || parser.NumIsLiteral curSymbol then
                    if Array.IndexOf(table.[getIndex()], curToken) <> -1 then
                        currentR = getNodeT curSymbol
                        currentIndex := !currentIndex + 1
                        currentPosition <- currentPosition + 1
                        condition <- false
(*
Обрабатываем нетерминал. Необходимо добавить новые контексты и обновить стек, чтобы знать, куда возвращаться. 
После этого управление передается диспетчеру.. 
*)
                elif parser.NumIsNonTerminal parser.Rules.[currentLabel.Value.Rule].[currentPosition] then 
                    let index = getIndex()
                    if Array.IndexOf(table.[index], curToken) <> -1 then
                        currentGSSNode = create (new Label(currentLabel.Value.Rule, currentPosition + 1)), currentGSSNode, currentIndex, currentN
                        for ruleN in table.[index] do
                            ignore <| (addContext !currentLabel, currentIndex, currentGSSNode, dummy)
                        condition <- true
////если же правило закончилось, то пришло время для попа и возврата дерева.
            else 
                currentN = getNodeP currentLabel, currentN, currentR /////нужно подумать, как менять позицию в лэйбле!!!
                pop !currentGSSNode !currentIndex currentN
                condition <- true
            if condition then dispatcher() else processing()
        processing()