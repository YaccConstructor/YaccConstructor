//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module YC.Parsing.RNGLR.Generator.States

open System.Collections.Generic
open YC.Parsing.Common.FinalGrammar
open YC.Generators.RNGLR.Generator.Graph

type OutTable = LR | LALR

type Kernel = int

type KernelInterpreter =
    static member inline toKernel (prod,pos) = (prod <<< 16) ||| pos
    static member inline incPos kernel = kernel + 1
    static member inline getProd kernel = kernel >>> 16
    static member inline getPos kernel = kernel &&& ((1 <<< 16) - 1)
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
        if grammar.rules.length rule = pos then
            grammar.indexator.eofIndex, Set.empty
        else
            let lookAheads =
                if grammar.epsilonTailStart.[rule] > pos + 1 then grammar.followSet.[rule].[pos]
                else Set.union grammar.followSet.[rule].[pos] endLookeheads
            grammar.rules.symbol rule pos, lookAheads

type StatesInterpreter (stateToVertex : Vertex<int,int>[], stateToKernels : Kernel[][], stateToLookahead : Set<int>[][]) =
    member this.count = stateToVertex.Length
    member this.vertex i = stateToVertex.[i]
    member this.kernels i = stateToKernels.[i]
    member this.lookaheads i = stateToLookahead.[i]
    
let buildStates outTable (grammar : FinalGrammar) = //(kernelIndexator : KernelIndexator) =
    let nextIndex, vertexCount =
        let num = ref -1
        (fun () -> incr num; !num)
        , (fun () -> !num + 1)
    let kernelsToVertex = new Dictionary<string, Vertex<int,int>>()
    let vertices = new ResizeArray<Vertex<int,int> >()
    let stateToKernels = new ResizeArray<Kernel[]>()
    let stateToLookahead = new ResizeArray<Set<int>[] >()
    let curSymbol kernel = KernelInterpreter.symbol grammar kernel
    let symbolAndLookAheads (*kernel lookAheads*) = KernelInterpreter.symbolAndLookAheads grammar
    let wasEdge = new ResizeArray<Set<int> >()
    let wasNonTerm = Array.zeroCreate grammar.indexator.fullCount
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
                    wasNonTerm.[nonTerm] <- true
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
                        let newSymbolSet = Set.difference symbolSet kernelToLookAhead.[kernel]
                        kernelToLookAhead.[kernel] <- Set.union kernelToLookAhead.[kernel] newSymbolSet
                        newSymbolSet
                if grammar.rules.length rule > 0 && (not newSymbolSet.IsEmpty || not wasNonTerm.[grammar.rules.symbol rule 0]) then
                    enqueue <| symbolAndLookAheads (kernel, newSymbolSet)
        for (f,s) in addedNTermsSymbols do
            wasNonTerm.[f] <- false
            wasNTermSymbol.[f,s] <- false
        addedNTermsSymbols.Clear()
        Array.ofSeq result
        |> (fun ks -> ks , ks |> Array.map (fun x -> kernelToLookAhead.[x]))

    let incount = ref 0
    let rec dfsLALR initKernelsAndLookAheads =
        incr incount
//        if !incount % 5000 = 0 then eprintf "%d " !incount
        let kernels,lookaheads = initKernelsAndLookAheads |> closure
        let key = String.concat "|" (kernels |> Array.map string)
        let vertex, newLookAheads, needDfs =
            if kernelsToVertex.ContainsKey key then
                let vertex = kernelsToVertex.[key]
                let alreadySets = stateToLookahead.[vertex.label]
                let mutable needDfs = false
                let diff = Array.zeroCreate kernels.Length
                for i = 0 to kernels.Length - 1 do
                    let diffSet = Set.difference lookaheads.[i] alreadySets.[i]
                    alreadySets.[i] <- Set.union alreadySets.[i] diffSet
                    if not diffSet.IsEmpty then needDfs <- true
                    diff.[i] <- diffSet
                vertex, diff, needDfs
            else
                let vertex = new Vertex<int,int>(nextIndex())
                wasEdge.Add Set.empty
                vertices.Add vertex
                kernelsToVertex.[key] <- vertex
                stateToKernels.Add kernels
                stateToLookahead.Add lookaheads
                vertex, lookaheads, true
        if needDfs then
            for i = 0 to grammar.indexator.fullCount - 1 do
                if i <> grammar.indexator.eofIndex then
                    let mutable newSymbols = false
                    let mutable count = 0
                    for j = 0 to kernels.Length-1 do
                        if curSymbol kernels.[j] = i then
                            if not newLookAheads.[j].IsEmpty then newSymbols <- true
                            count <- count + 1
                    if count > 0 && newSymbols then
                        let destStates = Array.zeroCreate count
                        let mutable curi = 0
                        for j = 0 to kernels.Length-1 do
                            if curSymbol kernels.[j] = i then
                                destStates.[curi] <- KernelInterpreter.incPos kernels.[j], newLookAheads.[j]
                                curi <- curi + 1
                        let newVertex : Vertex<_,_> = dfsLALR destStates
                        if not <| wasEdge.[vertex.label].Contains newVertex.label then
                            wasEdge.[vertex.label] <- wasEdge.[vertex.label].Add newVertex.label
                            vertex.addEdge <| new Edge<_,_>(newVertex, i)
        vertex

    let rec dfsLR initKernelsAndLookAheads =
        incr incount
//        if !incount % 5000 = 0 then eprintf "%d " !incount
        let kernels,lookaheads = initKernelsAndLookAheads |> closure
        let setToStr = Set.map (sprintf "%d") >> String.concat ","
        let key = String.concat "|" (Array.map2 (fun x y -> sprintf "%d(%s)" x (setToStr y)) kernels lookaheads)
        //printfn "%s" key
        if kernelsToVertex.ContainsKey key then
            kernelsToVertex.[key]
        else
            let vertex = new Vertex<int,int>(nextIndex())
            //wasEdge.Add Set.empty
            vertices.Add vertex
            kernelsToVertex.[key] <- vertex
            stateToKernels.Add kernels
            stateToLookahead.Add lookaheads
            for i = 0 to grammar.indexator.fullCount - 1 do
                if i <> grammar.indexator.eofIndex then
                    let mutable count = 0
                    for j = 0 to kernels.Length-1 do
                        if curSymbol kernels.[j] = i && not lookaheads.[j].IsEmpty then
                            count <- count + 1
                    if count > 0 then
                        let destStates = Array.zeroCreate count
                        let mutable curi = 0
                        for j = 0 to kernels.Length-1 do
                            if curSymbol kernels.[j] = i && not lookaheads.[j].IsEmpty then
                                destStates.[curi] <- KernelInterpreter.incPos kernels.[j], lookaheads.[j]
                                curi <- curi + 1
                        let newVertex : Vertex<_,_> = dfsLR destStates
                        //wasEdge.[vertex.label] <- wasEdge.[vertex.label].Add newVertex.label
                        vertex.addEdge <| new Edge<_,_>(newVertex, i)
            vertex
    let initKernel = KernelInterpreter.toKernel(grammar.startRule, 0)
    let initLookAhead = Set.ofSeq [grammar.indexator.eofIndex]
    [| initKernel, initLookAhead|]
    |> match outTable with
        | LALR -> dfsLALR
        | LR -> dfsLR
    |> ignore
    eprintfn "Dfs calls count: %d" !incount
    eprintfn "States count: %d" <| vertexCount()
    //printfn "rules count = %d; states count = %d" grammar.rules.rulesCount <| vertexCount()
        (*
    let printSymbol (symbol : int) =
        if symbol < grammar.indexator.nonTermCount then
            grammar.indexator.indexToNonTerm symbol
        elif symbol >= grammar.indexator.termsStart && symbol <= grammar.indexator.termsEnd then
            grammar.indexator.indexToTerm symbol
        else grammar.indexator.indexToLiteral symbol
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
        let vertex = vertices.[i]
        for edge in vertex.outEdges do
            printf "(%s,%d) " (printSymbol edge.label) edge.dest.label
        printfn ""*)
    new StatesInterpreter(vertices.ToArray(), stateToKernels.ToArray(), stateToLookahead.ToArray())
