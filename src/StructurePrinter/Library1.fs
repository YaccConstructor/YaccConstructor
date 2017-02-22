module StructurePrinter

open Yard.Generators.GLL.ParserCommon
type SysDict<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>

type CompressedArray<'t> = Yard.Generators.GLL.ParserCommon.CompressedArray<'t>

type EdgeOfGSS = 
    {
        startVertex : GSSVertexNFA
        endVertex : GSSVertexNFA
        state : int<state>
        len : uint16
    }

/// |vertex(0,1)| --- stateToContinue(2), length(5) ---> |vertex(state(3),pos(4))|
let printGSS (edges : (*0*)(*1*)CompressedArray<SysDict<int<state>(*2*),SysDict<int<state>(*3*),(int<positionInInput>(*4*) * uint16(*5*)) []>>> []) =
    let outEdges = edges.[int(startVertex.NontermState)].[startVertex.PositionInInput]

    let cond, dict = 
        if outEdges <> null
        then
            let cond, dictStateKey = outEdges.TryGetValue state
            if cond
            then
                let cond, posLen = dictStateKey.TryGetValue endVertex.NontermState
                if cond
                then
                    if posLen |> Array.contains (endVertex.PositionInInput,len) 
                    then
                        true, None
                    else
                        let newPosLen = Array.append posLen [|endVertex.PositionInInput,len|]
                        dictStateKey.[endVertex.NontermState] <- newPosLen
                        false, None
                else
                    let arr = [|endVertex.PositionInInput,len|]
                    dictStateKey.Add(endVertex.NontermState, arr)
                    false, None 
            else
                let d1 = new SysDict<int<state>, _>()
                let arr = [|endVertex.PositionInInput,len|]
                d1.Add(endVertex.NontermState, arr)
                outEdges.Add(state, d1)
                false, None
        else 
            let d1 = new SysDict<int<_>, SysDict<int<state>, _>>()
            let d2 = new SysDict<int<state>, _>()
            let arr = [|endVertex.PositionInInput,len|]
            d2.Add(endVertex.NontermState, arr)
            d1.Add(state, d2)
            false, Some d1
    if dict.IsSome then edges.[int startVertex.NontermState].[startVertex.PositionInInput] <- dict.Value
    cond
    
    edges
    |> Array.iteri (fun state smth0 -> 
        smth0
        |> )
        let 
        for in 