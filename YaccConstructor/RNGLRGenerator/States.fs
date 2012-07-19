//  States.fs builds graph, related with states of parser
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Yard.Generators.RNGLR.States

open System.Collections.Generic
open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR

type Kernel = int
//type Item = Kernel * Set<int>

type KernelInterpreter =
    static member inline toKernel (prod,pos) = (prod <<< 16) ||| pos
    static member inline incPos kernel = kernel + 1
    static member inline getProd kernel = kernel >>> 16
    static member inline getPos kernel = kernel <<< 16 >>> 16
    static member inline unzip kernel = (KernelInterpreter.getProd kernel, KernelInterpreter.getPos kernel)
    static member inline kernelsOfState = fst
    static member inline lookAheadsOfState = snd

    static member inline symbol (grammar : FinalGrammar) kernel =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        if grammar.rules.length rule = pos then grammar.indexator.eofIndex
        else grammar.rules.symbol rule pos

    static member inline symbolAndLookAheads (grammar : FinalGrammar) (kernel, endLookeheads) =
        let rule = KernelInterpreter.getProd kernel
        let pos = KernelInterpreter.getPos kernel
        if grammar.rules.length rule = pos then (grammar.indexator.eofIndex, Set.empty)
        else
            let lookAheads =
                if grammar.epsilonTailStart.[rule] > pos + 1 then grammar.followSet.[rule].[pos]
                else Set.union grammar.followSet.[rule].[pos] endLookeheads
            (grammar.rules.symbol rule pos, lookAheads)
(*
type KernelIndexator (grammar : FinalGrammar) =
    let kernels = 
        let initKernel = KernelInterpreter.toKernel(grammar.startRule, 0)
        let was : bool[] = Array.zeroCreate grammar.indexator.fullCount
        let rec dfs nonTerminal =
            was.[nonTerminal] <- true
            for rule in grammar.rules.rulesWithLeftSide nonTerminal do
                for symbol in grammar.rules.rightSide rule do
                    if (not was.[symbol]) then
                        dfs symbol
                
        dfs grammar.rules.startSymbol
        [|for i in 0..grammar.indexator.fullCount-1 do
            if was.[i] then
                for rule in grammar.rules.rulesWithLeftSide i do
                    for pos = 0 to grammar.rules.length rule - 1 do
                        yield KernelInterpreter.toKernel (rule, pos)
        |]

    let kernelToIdx = kernels |> Array.mapi (fun i x -> (x,i)) |> dict

    member this.indexToKernel index = kernels.[index]
    member this.kernelToIndex kernel = kernelToIdx.Item kernel
*)
type StatesInterpreter (stateToVertex : Vertex<int,int>[], stateToKernels : Kernel[][], stateToLookahead : Set<int>[][]) =
    member this.count = stateToVertex.Length
    member this.vertex i = stateToVertex.[i]
    member this.kernels i = stateToKernels.[i]
    member this.lookaheads i = stateToLookahead.[i]
    
let buildStates (grammar : FinalGrammar) = //(kernelIndexator : KernelIndexator) =
    let nextIndex, vertexCount =
        let num = ref -1
        (fun () -> incr num; !num)
        , (fun () -> !num + 1)
    let kernelsToVertex = new Dictionary<string, Vertex<int,int>>()
    let virtices = new ResizeArray<Vertex<int,int> >()
    let stateToKernels = new ResizeArray<Kernel[]>()
    let stateToLookahead = new ResizeArray<Set<int>[] >()
    let curSymbol kernel = KernelInterpreter.symbol grammar kernel
    let symbolAndLookAheads (*kernel lookAheads*) = KernelInterpreter.symbolAndLookAheads grammar
    let wasEdge = new ResizeArray<Set<int> >()
    let wasNTermSymbol : bool[,] = Array2D.zeroCreate grammar.indexator.fullCount grammar.indexator.fullCount
    let addedNTermsSymbols = new ResizeArray<_>(grammar.indexator.fullCount * grammar.indexator.fullCount)

    let closure (kernelsAndLookAheads : (Kernel * Set<int>)[]) =
        //eprintf "%d " <| addedNTermsSymbols.Capacity
        let mutable result = kernelsAndLookAheads |> Array.map fst |> Set.ofArray
        let kernelToLookAhead = new Dictionary<_,_> ()// Array.zip kernels lookAheads |> dict
        let queue = new Queue<_>()
        let enqueue (nonTerm, symbolSet) = 
            let checkWas symbol =
                if not wasNTermSymbol.[nonTerm, symbol] then
                    wasNTermSymbol.[nonTerm, symbol] <- true
                    addedNTermsSymbols.Add(nonTerm, symbol)
                    true
                else false
            let newSymbolSet = Set.filter checkWas symbolSet
            queue.Enqueue (nonTerm, newSymbolSet)
        for i = 0 to kernelsAndLookAheads.Length - 1 do
            kernelToLookAhead.Add (kernelsAndLookAheads.[i])
            enqueue <| symbolAndLookAheads kernelsAndLookAheads.[i]
        while queue.Count > 0 do
            let nonterm, symbolSet = queue.Dequeue()
            for rule in grammar.rules.rulesWithLeftSide nonterm do
                let kernel = KernelInterpreter.toKernel (rule,0)
                let newSymbolSet = 
                    if not <| result.Contains kernel then
                        result <- result.Add kernel
                        kernelToLookAhead.Add(kernel, symbolSet)
                        symbolSet
                    else
                        let newSymbolSet = Set.difference symbolSet (kernelToLookAhead.Item kernel)
                        kernelToLookAhead.Item kernel <- Set.union (kernelToLookAhead.Item kernel) newSymbolSet
                        newSymbolSet
                if grammar.rules.length rule > 0 && not newSymbolSet.IsEmpty then
                    enqueue <| symbolAndLookAheads (KernelInterpreter.toKernel(rule,0), newSymbolSet)
        for pair in addedNTermsSymbols do
            wasNTermSymbol.[fst pair, snd pair] <- false
        addedNTermsSymbols.Clear()
        Array.ofSeq result
        |> (fun ks -> ks , [|for i = 0 to ks.Length - 1 do yield kernelToLookAhead.Item ks.[i]|])
    let incount = ref 0
    let rec dfs initKernelsAndLookAheads =
        incr incount
        if !incount % 100 = 0 then eprintf "%d " !incount
        let kernels,lookaheads = initKernelsAndLookAheads |> closure
        let key = String.concat "|" (kernels |> Array.map (sprintf "%d"))
        let vertex, newLookAheads, needDfs =
            if kernelsToVertex.ContainsKey key then
                let vertex = kernelsToVertex.Item key
                let alreadySets = stateToLookahead.Item vertex.label
                let needDfs = ref false
//                printfn "============"
//                printfn "%A" alreadySets
                let diff =
                    [|for i = 0 to kernels.Length - 1 do
                        let diffSet = Set.difference lookaheads.[i] alreadySets.[i]
                        alreadySets.[i] <- Set.union alreadySets.[i] diffSet
                        if not diffSet.IsEmpty then needDfs := true
//                        for s in diffSet do
//                            System.Diagnostics.Debug.Assert <| (stateToLookahead.Item vertex.label).[i].Contains s
                        yield diffSet|]
                vertex, diff, !needDfs
            else
                //printfn "%A" <| key
                let vertex = new Vertex<int,int>(nextIndex())
                wasEdge.Add Set.empty
                virtices.Add vertex
                kernelsToVertex.Add(key, vertex)
                //System.Diagnostics.Debug.Assert(result.ContainsKey key)
                stateToKernels.Add(kernels)
                stateToLookahead.Add(lookaheads)
                vertex, lookaheads, true
                // adding edges
        if needDfs then
//            printfn "%d" vertex.label
//            printfn "%A" kernels
//            printfn "%A" newLookAheads
            for i = 0 to grammar.indexator.fullCount - 1 do
                if i <> grammar.indexator.eofIndex then
                    let newSymbols = ref false
                    let destStates =
                        [|for j = 0 to kernels.Length-1 do
                            if curSymbol kernels.[j] = i then
                                if not newLookAheads.[j].IsEmpty then newSymbols := true
                                yield (KernelInterpreter.incPos kernels.[j], newLookAheads.[j])
                        |]
                    if destStates.Length > 0 && !newSymbols then
                        let newVertex : Vertex<_,_> = dfs destStates
                        if not <| wasEdge.[vertex.label].Contains newVertex.label then
                            wasEdge.[vertex.label] <- wasEdge.[vertex.label].Add newVertex.label
                            vertex.addEdge(new Edge<_,_>(newVertex, i))
        vertex
    let initKernel = KernelInterpreter.toKernel(grammar.startRule, 0)
    let initLookAhead = Set.ofSeq [grammar.indexator.eofIndex]
    [| initKernel, initLookAhead|] |> dfs |> ignore

    //printfn "rules count = %d; states count = %d" grammar.rules.rulesCount <| vertexCount()
    let printSymbol (symbol : int) =
        if symbol < grammar.indexator.nonTermCount then
            grammar.indexator.indexToNonTerm symbol
        elif symbol >= grammar.indexator.termsStart && symbol <= grammar.indexator.termsEnd then
            grammar.indexator.indexToTerm symbol
        else grammar.indexator.indexToLiteral symbol
    let print () =
        printfn "\nrules:"
        for i = 0 to grammar.rules.rulesCount-1 do
            printf "%4d: %s = " i <| printSymbol (grammar.rules.leftSide i)
            for j = 0 to grammar.rules.length i - 1 do
                printf "%s " <| printSymbol (grammar.rules.symbol i j)
            printfn ""
        printfn "\nstates:"
        for i = 0 to vertexCount()-1 do
            printfn "==============================\n%d:" i
            let kernels = stateToKernels.[i]
            let lookaheads = stateToLookahead.[i]
            for k = 0 to kernels.Length-1 do
                printfn "(%d,%d) [%s]" (KernelInterpreter.getProd kernels.[k]) (KernelInterpreter.getPos kernels.[k])
                    <| (lookaheads.[k] |> List.ofSeq
                        |> List.map (fun x -> printSymbol x)
                        |> String.concat "," )
            printfn "------------------------------"
            let vertex = virtices.[i]
            for edge in vertex.outEdges do
                printf "(%s,%d) " (printSymbol edge.label) edge.dest.label
            printfn ""
    print ()
    new StatesInterpreter(virtices.ToArray(), stateToKernels.ToArray(), stateToLookahead.ToArray())
