module TrieToFormula
open QuickGraph

type FormulaNode = 
    | OR of FormulaNode []
    | AND of FormulaNode []
    | XOR of FormulaNode []
    | NOT of FormulaNode
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
        | VAR(x) -> x
        | NONE -> ""



        

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