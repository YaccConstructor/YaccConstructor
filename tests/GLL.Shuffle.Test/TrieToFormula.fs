module TrieToFormula
open QuickGraph
open Yard.Generators.Common.ASTGLLFSA
open AbstractAnalysis.Common
open Yard.Core.Namer

type FormulaNode = 
    | OR of FormulaNode []
    | AND of FormulaNode []
    | XOR of FormulaNode []
    | NOT of FormulaNode
    | EQ of FormulaNode * FormulaNode
    | VAR of string
    | NONE
    override this.ToString() = 
        let br x = "(" + x.ToString() + ")"
        let arrToLine (arr : 'a []) (symb : string) = 
            if arr.Length > 1
            then
                arr
                |> Array.mapi (fun i x -> if i <> 0 then symb + br x
                                          else br x)
                |> (fun x -> 
                        let s = new System.Text.StringBuilder()
                        for it in x do 
                            if (it <> symb + "()") then s.Append( it) |> ignore
                        s.ToString())
            else
                br arr.[0]
                                            
        match this with
        | OR(x) -> arrToLine x "|"
        | AND(x) -> arrToLine x "&"
        | XOR(x) -> arrToLine x "XOR"
        | NOT(x) -> "~" + x.ToString()
        | EQ(x,y) -> x.ToString() + "==" + y.ToString()
        | VAR(x) -> x
        | NONE -> ""


 let rec reduceFormula = function 
    | OR(l) ->
        let r = 
            l
            |> Array.map reduceFormula
            |> Array.filter(fun x -> x = NONE)
        if (r.Length <> 0)
        then OR(r)
        else NONE
    | AND(l) ->
        let r = 
            l
            |> Array.map reduceFormula
            |> Array.filter(fun x -> x = NONE)
        if (r.Length <> 0)
        then AND(r)
        else NONE
    | XOR(l) ->
        let r = 
            l
            |> Array.map reduceFormula
            |> Array.filter(fun x -> x = NONE)
        if (r.Length <> 0)
        then XOR(r)
        else NONE
    | NOT(l) ->
        let r = reduceFormula l
        if (r <> NONE)
        then NOT(r)
        else NONE
    | EQ(l,r) ->
        let lRes = reduceFormula l
        let rRes = reduceFormula r
        if (lRes = NONE) || (rRes = NONE)
        then failwith "equaliti none"
        else EQ(lRes,rRes)
    | VAR(s) ->
        VAR(s)
    | NONE -> NONE
    

        

let trieToFormula (trie : AdjacencyGraph<int,TaggedEdge<int,int>>) (beginning : int) (input : string[] ) (trieNumber : int)=
    let vars = 
        input
        |> Array.mapi(fun i s -> s + "_i" + (i.ToString()) +  "_" + trieNumber.ToString())

    //let getUnusedVarsConj (setOfVars : int Set) : FormulaNode =
    //    let n = setOfVars.Count

    //    if n > 1
    //    then
    //        setOfVars
    //        |> Seq.map( fun x -> vars.[x] |> VAR |> NOT)
    //        |> Array.ofSeq
    //        |> AND
            
    //    elif n = 1
    //    then
    //        VAR(vars.[setOfVars |> Array.ofSeq |> (fun x -> x.[0])])
    //    else
    //        NONE

    let rec processsVert (v : int) : FormulaNode = 
        let outEdges = trie.OutEdges(v) |> Array.ofSeq
        if outEdges.Length > 1
        then
            let node = Array.init outEdges.Length (fun i -> [| vars.[outEdges.[i].Tag] |> VAR; processsVert outEdges.[i].Target |] |> AND) |> OR
            node
        elif outEdges.Length = 1
        then
            [| vars.[outEdges.[0].Tag] |> VAR; processsVert outEdges.[0].Target |] |> AND
        else
        //    getUnusedVarsConj setOfVars   
            NONE
            
    let formula = processsVert beginning

    formula, vars





let sppfRootsToFormula (roots : INode[]) (intToString : int<token> -> string) (i : int) = // (beginning : int) (input : string[] ) (trieNumber : int)=
    let rec nodeProc : INode -> FormulaNode = function
    | :? TerminalNode as n ->      
        if n.Name = -1<token>
        then
            NONE
        else
            let posl = getLeftExtension n.Extension
            let posr = getRightExtension n.Extension
            FormulaNode.VAR(posl.ToString() + intToString n.Name + posr.ToString() + "_" + i.ToString())          
    | :? IntermidiateNode as n ->
        let first = nodeProc n.First
        if (n.Others <> null)
        then
            let others = 
                n.Others
                |> Seq.map (fun x -> nodeProc x)
                |> Array.ofSeq
            OR(Array.append others [|first|])
        else
            first
    | :? PackedNode as n ->
        AND([|nodeProc n.Left; nodeProc n.Right|])
    | :? NonTerminalNode as n ->
        let first = nodeProc n.First
        if (n.Others <> null)
        then
            let others = 
                n.Others
                |> Seq.map (fun x -> nodeProc x)
                |> Array.ofSeq
            OR(Array.append others [|first|])
        else
            first
    | _ -> failwith "unexpected type of node"

    let formula = 
        roots
        |> Array.map (fun x -> nodeProc x)
        |> (fun x -> OR(x))

    formula

let inputGraphToFormula (graph : SimpleInputGraph<string>) = 
    graph.Vertices
    |> Seq.map(fun vert ->
        let outEdgesVars = 
            graph.OutEdges(vert)
            |> Seq.map(fun x -> VAR(x.Source.ToString() + x.Tag + x.Target.ToString()))
            |> Array.ofSeq
        let inEdgesVars = 
            graph.Edges
            |> Seq.filter(fun x -> x.Target = vert)
            |> Seq.map(fun x -> VAR(x.Source.ToString() + x.Tag + x.Target.ToString())) 
            |> Array.ofSeq

        EQ(XOR(outEdgesVars), XOR(inEdgesVars)))
    |> Array.ofSeq
    |> (fun x -> AND(x))

let edgesMapping (edges : ParserEdge<string>[]) (count : int) =
    edges
    |> Array.map(fun x -> 
        Array.init count (fun i -> VAR(x.Source.ToString() + x.Tag + x.Target.ToString() + "_" + i.ToString()))
        |> (fun vars -> EQ( XOR(vars), VAR(x.Source.ToString() + x.Tag + x.Target.ToString()) ) )
        )
    |> (fun x -> AND(x))