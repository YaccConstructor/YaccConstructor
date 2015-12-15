module Yard.Generators.RNGLR.ReadBack.States

open System.Collections.Generic
open Yard.Generators.Common
open Yard.Generators.Common.EBNF.FinalGrammar
open Yard.Generators.Common.LR.Kernels
open Yard.Generators.Common.LR.NFA

type StatesInterpreter(stateToVertex : Vertex<int,int>[], stateToKernels : Kernel[][], stateToLookahead : Set<int>[][]) =
    member this.count = stateToVertex.Length
    member this.vertex i = stateToVertex.[i]
    member this.kernels i = stateToKernels.[i]
    member this.lookaheads i = stateToLookahead.[i]
    
let buildStatesNFA outTable (grammar : FinalGrammarNFA) = //(kernelIndexator : KernelIndexator) =
    let nextIndex, vertexCount =
        let num = ref -1
        (fun () -> incr num; !num)
        , (fun () -> !num + 1)
    let kernelsToVertex = new Dictionary<string, Vertex<int,int>>()
    let vertices = new ResizeArray<Vertex<int,int> >()
    let stateToKernels = new ResizeArray<Kernel[]>()
    let stateToLookahead = new ResizeArray<Set<int>[] >()
    let curSymbol kernel = KernelInterpreterNFA.symbol grammar kernel
    let symbolAndLookAheads (*kernel lookAheads*) = KernelInterpreterNFA.symbolAndLookAheads grammar
    let wasEdge = new ResizeArray<Set<int> >()
    let wasNonTerm = Array.zeroCreate grammar.indexator.fullCount
    let wasNTermSymbol : bool[,] = Array2D.zeroCreate grammar.indexator.fullCount grammar.indexator.fullCount
    let addedNTermsSymbols = new ResizeArray<_>(grammar.indexator.fullCount * grammar.indexator.fullCount)

    //let framesDFS = new ResizeArray<int * GrammarSymbol * FrameEBNF []>() // DELETE

    let closure (kernelsAndLookaheads : (Kernel * Set<int>)[]) =
        //eprintf "%d " <| addedNTermsSymbols.Capacity
        let mutable result = kernelsAndLookaheads |> Array.map fst |> Set.ofArray
        let kernelToLookahead = new Dictionary<_,_> ()// Array.zip kernels lookAheads |> dict
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
        for i = 0 to kernelsAndLookaheads.Length - 1 do
            if (fst kernelsAndLookaheads.[i] |> kernelToLookahead.ContainsKey |> not) then
                kernelToLookahead.Add (kernelsAndLookaheads.[i])
            else
                kernelToLookahead.[fst kernelsAndLookaheads.[i]] 
                    <- Set.union kernelToLookahead.[fst kernelsAndLookaheads.[i]] (snd kernelsAndLookaheads.[i])
            enqueue <| symbolAndLookAheads kernelsAndLookaheads.[i]
        while queue.Count > 0 do
            let nonterm, symbolSet = queue.Dequeue()
            for rule in grammar.rules.rulesWithLeftSide nonterm do
                let kernelsAndStarts = grammar.startPositions.[rule] |> Set.map (fun x -> KernelInterpreter.toKernel (rule,x), x)
                for (kernel, startPosition) in kernelsAndStarts do                
                    let newSymbolSet = 
                        if not <| result.Contains kernel then
                            result <- result.Add kernel
                            kernelToLookahead.Add(kernel, symbolSet)
                            symbolSet
                        else
                            let newSymbolSet = Set.difference symbolSet kernelToLookahead.[kernel]
                            kernelToLookahead.[kernel] <- Set.union kernelToLookahead.[kernel] newSymbolSet
                            newSymbolSet
                    let ruleStartSymbol = grammar.rules.symbol rule startPosition
                    if ruleStartSymbol <> grammar.indexator.epsilonIndex && (not newSymbolSet.IsEmpty || not wasNonTerm.[ruleStartSymbol]) then
                        enqueue <| symbolAndLookAheads (kernel, newSymbolSet)
        for (f,s) in addedNTermsSymbols do
            wasNonTerm.[f] <- false
            wasNTermSymbol.[f,s] <- false
        addedNTermsSymbols.Clear()
        Array.ofSeq result
        |> (fun ks -> ks , ks |> Array.map (fun x -> kernelToLookahead.[x]))

    let incount = ref 0
    let dfsDepth = ref 0
    let maxDfsDepth = ref 0
    let rec dfsLALR initKernelsAndLookAheads =
        incr incount
        //if !incount % 100 = 0 then eprintf "%d " !incount
        incr dfsDepth
        if !dfsDepth > !maxDfsDepth then
            incr maxDfsDepth
            if !maxDfsDepth % 100000 = 0 then
                eprintf "depth = %d\n " !maxDfsDepth
        let kernels, lookaheads = initKernelsAndLookAheads |> closure
        let key = String.concat "|" (kernels |> Array.map string)
        let vertex, newLookaheads, needDfs =
            if kernelsToVertex.ContainsKey key then
                let vertex = kernelsToVertex.[key]
                let alreadySets = stateToLookahead.[vertex.label]
                let mutable needDfs = false
                let diff = Array.zeroCreate kernels.Length
                for i = 0 to kernels.Length - 1 do
                    let diffSet = Set.difference lookaheads.[i] alreadySets.[i]
                    alreadySets.[i] <- Set.union alreadySets.[i] diffSet
                    if not diffSet.IsEmpty then
                        needDfs <- true
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
                    //check that at least one current kernel has new lookaheads
                    let mutable hasNewLookahead = false
                    for j = 0 to kernels.Length - 1 do
                        if curSymbol kernels.[j] = i && not newLookaheads.[j].IsEmpty then hasNewLookahead <- true
                    let destStates = new Dictionary<int, Set<int>>()
                    if hasNewLookahead then
                        let mutable stackSet = Set.empty
                        for j = 0 to kernels.Length-1 do
                            if curSymbol kernels.[j] = i then
                                let nextKernels = KernelInterpreterNFA.nextPos grammar kernels.[j]
                                for nextKernel in nextKernels do
                                    if not <| destStates.ContainsKey nextKernel then
                                        destStates.[nextKernel] <- newLookaheads.[j]
                                    else
                                        destStates.[nextKernel] <- Set.union destStates.[nextKernel] newLookaheads.[j]
                        //if destStates.Count <> 0 then
                        let destStates_arr = Array.zeroCreate destStates.Count
                        let dsIter = ref 0
                        for destState in destStates do
                            destStates_arr.[!dsIter] <- destState.Key, destState.Value
                            incr dsIter
                        let newVertex : Vertex<_,_> = dfsLALR destStates_arr
                        if not <| wasEdge.[vertex.label].Contains i then
                            wasEdge.[vertex.label] <- wasEdge.[vertex.label].Add i
                            vertex.addEdge <| new Edge<_,_>(newVertex, i)
        decr dfsDepth
        vertex

    let rec dfsLR initKernelsAndLookAheads =
        incr incount
        incr dfsDepth
        (*if !dfsDepth > !maxDfsDepth then
            incr maxDfsDepth
            if !maxDfsDepth % 100000 = 0 then
                eprintf "depth = %d\n " !maxDfsDepth
        if !incount % 1000000 = 0 then 
            eprintf " DFS%d vertices = %d\n " !incount vertices.Count*)
        let kernels, lookaheads = initKernelsAndLookAheads |> closure
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
                    let destStates = new Dictionary<int, Set<int>>()
                    for j = 0 to kernels.Length-1 do
                            if curSymbol kernels.[j] = i && not lookaheads.[j].IsEmpty then
                                let nextKernels = KernelInterpreterNFA.nextPos grammar kernels.[j]
                                for nextKernel in nextKernels do
                                    if not <| destStates.ContainsKey nextKernel then
                                        destStates.[nextKernel] <- lookaheads.[j]
                                    else
                                        destStates.[nextKernel] <- Set.union destStates.[nextKernel] lookaheads.[j]
                    if destStates.Count <> 0 then
                        let destStates_arr = Array.zeroCreate destStates.Count
                        let dsIter = ref 0
                        for destState in destStates do
                            destStates_arr.[!dsIter] <- destState.Key, destState.Value
                            incr dsIter
                        let newVertex : Vertex<_,_> = destStates_arr |> dfsLR
                        vertex.addEdge <| new Edge<_,_>(newVertex, i)
            decr dfsDepth
            vertex

    let initKernels = grammar.startPositions.[grammar.startRule] |> Set.map (fun x -> KernelInterpreter.toKernel(grammar.startRule, x))
    let initLookAhead = Set.ofSeq [grammar.indexator.eofIndex]
    let initKernelsAndLookAheads = Set.map (fun x -> (x, initLookAhead)) initKernels |> Set.toArray
    (*let threadFun = fun () ->
        initKernelsAndLookAheads
        |> match outTable with
            | LALR -> dfsLALR
            | LR -> dfsLR
        |> ignore
    let callStackSize = 10 * (1 <<< 20)
    let threadDfs = new Thread(threadFun, callStackSize)
    threadDfs.Start()
    threadDfs.Join()*)
    initKernelsAndLookAheads
        |> match outTable with
            | LALR -> dfsLALR
            | LR -> dfsLR
        |> ignore
    eprintfn "maxDfsDepth %d" !maxDfsDepth
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