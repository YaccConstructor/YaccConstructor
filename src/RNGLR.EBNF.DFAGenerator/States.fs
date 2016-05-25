module Yard.Generators.RNGLR.EBNF.DFA.States

open System.Collections.Generic
open Yard.Generators.Common.DFA.FinalGrammar
open Yard.Generators.Common
open Yard.Generators.RNGLR.States
open Yard.Generators.Common.LR.Kernels
open Yard.Generators.Common.LR.DFA
open Yard.Generators.Common.EBNF
open System.IO //DELETE
open System.Threading // DELETE

//type Item = Kernel * Set<int>
type StatesInterpreter(stateToVertex : Vertex<int,int>[], stateToMainKernels : Kernel[][], stateToMainLookahead : Set<int>[][], stateToDerivedKernels : Kernel[][], stateToDerivedLookahead : Set<int>[][]) =
    member this.count = stateToVertex.Length
    member this.vertex i = stateToVertex.[i]
    member this.mainKernels i = stateToMainKernels.[i]
    member this.mainLookaheads i = stateToMainLookahead.[i]
    member this.derivedKernels i = stateToDerivedKernels.[i]
    member this.derivedLookaheads i = stateToDerivedLookahead.[i]
    
let buildStatesDFA outTable (grammar : FinalGrammarDFA) =
    let nextIndex, vertexCount =
        let num = ref -1
        (fun () -> incr num; !num)
        , (fun () -> !num + 1)
    let kernelsToVertex = new Dictionary<string, Vertex<int,int>>()
    let vertices = new ResizeArray<Vertex<int,int> >()
    let stateToMainKernels = new ResizeArray<Kernel[]>()
    let stateToMainLookahead = new ResizeArray<Set<int>[]>()
    let stateToDerivedKernels = new ResizeArray<Kernel[]>()
    let stateToDerivedLookahead = new ResizeArray<Set<int>[]>()
    let curSymbolsAndNextPos kernel = KernelInterpreterDFA.symbolsAndNextPos grammar kernel
    let symbolsAndLookaheads (*kernel lookAheads*) = KernelInterpreterDFA.symbolsAndLookaheads grammar
    let wasEdge = new ResizeArray<Set<int> >()
    let wasNonTerm = Array.zeroCreate grammar.indexator.fullCount
    let wasNTermSymbol : bool[,] = Array2D.zeroCreate grammar.indexator.fullCount grammar.indexator.fullCount
    let addedNTermsSymbols = new ResizeArray<_>(grammar.indexator.fullCount * grammar.indexator.fullCount)

    //let framesDFS = new ResizeArray<int * GrammarSymbol * FrameEBNF []>() // DELETE

    let closure (mainItems : Item[]) =
        //eprintf "%d " <| addedNTermsSymbols.Capacity
        let mutable resultMain = mainItems |> Array.map fst |> Set.ofArray
        let mutable resultDerived : Set<Kernel>  = Set.empty
        let mainKernelToLookAhead = new Dictionary<_,_> ()// Array.zip kernels lookAheads |> dict
        let derivedKernelToLookAhead = new Dictionary<_,_> ()
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
        for i = 0 to mainItems.Length - 1 do
            if (fst mainItems.[i] |> mainKernelToLookAhead.ContainsKey |> not) then
                mainKernelToLookAhead.Add (mainItems.[i])
            else
                mainKernelToLookAhead.[fst mainItems.[i]] 
                    <- Set.union mainKernelToLookAhead.[fst mainItems.[i]] (snd mainItems.[i])
            Array.iter enqueue <| symbolsAndLookaheads mainItems.[i]
        while queue.Count > 0 do
            let symbol, symbolSet = queue.Dequeue()
            let rule = grammar.rules.ruleWithLeftSide symbol
            if rule >= 0 then
                let nonTerm = symbol
                let startPositions = grammar.rules.startPos rule
                let kernels = startPositions |> Array.map (fun x -> KernelInterpreter.toKernel (rule, x))
                for kernel in kernels do
                    let newSymbolSet = 
                        if not <| resultDerived.Contains kernel then
                            resultDerived <- resultDerived.Add kernel
                            derivedKernelToLookAhead.Add(kernel, symbolSet)
                            symbolSet
                        else
                            let newSymbolSet = Set.difference symbolSet derivedKernelToLookAhead.[kernel]
                            derivedKernelToLookAhead.[kernel] <- Set.union derivedKernelToLookAhead.[kernel] newSymbolSet
                            newSymbolSet
                    symbolsAndLookaheads (kernel, newSymbolSet)
                    |> Array.iter 
                        (fun x ->
                            let (symbol, _) = x
                            if not newSymbolSet.IsEmpty || not wasNonTerm.[symbol] then
                               enqueue x
                        )
        for (f,s) in addedNTermsSymbols do
            wasNonTerm.[f] <- false
            wasNTermSymbol.[f,s] <- false
        addedNTermsSymbols.Clear()
        (Array.ofSeq resultMain, Array.ofSeq resultDerived)
        |> (fun (mks, dks) -> mks , mks |> Array.map (fun x -> mainKernelToLookAhead.[x]), dks , dks |> Array.map (fun x -> derivedKernelToLookAhead.[x]))

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
        let mainKernels, mainLookaheads, derivedKernels, derivedLookaheads = initKernelsAndLookAheads |> closure
        let key = String.concat "|" (*Set.union (Set.ofArray mainKernels) (Set.ofArray derivedKernels) |> Set.toArray |> Array.sort |> Array.map string*)(mainKernels |> Array.map string)
        let vertex, newMainLookaheads, newDerivedLookaheads, needDfs =
            if kernelsToVertex.ContainsKey key then
                let vertex = kernelsToVertex.[key]
                let alreadySetsMain = stateToMainLookahead.[vertex.label]
                let alreadySetsDerived = stateToDerivedLookahead.[vertex.label]
                let mutable needDfs = false
                let diffMain = Array.zeroCreate mainKernels.Length
                let diffDerived = Array.zeroCreate derivedKernels.Length
                for i = 0 to mainKernels.Length - 1 do
                    let diffSet = Set.difference mainLookaheads.[i] alreadySetsMain.[i]
                    alreadySetsMain.[i] <- Set.union alreadySetsMain.[i] diffSet
                    if not diffSet.IsEmpty then
                        needDfs <- true
                    diffMain.[i] <- diffSet
                for i = 0 to derivedKernels.Length - 1 do
                    let diffSet = Set.difference derivedLookaheads.[i] alreadySetsDerived.[i]
                    alreadySetsDerived.[i] <- Set.union alreadySetsDerived.[i] diffSet
                    if not diffSet.IsEmpty then needDfs <- true
                    diffDerived.[i] <- diffSet
                vertex, diffMain, diffDerived, needDfs
            else
                let vertex = new Vertex<int,int>(nextIndex())
                wasEdge.Add Set.empty
                vertices.Add vertex
                kernelsToVertex.[key] <- vertex
                stateToMainKernels.Add mainKernels
                stateToMainLookahead.Add mainLookaheads
                stateToDerivedKernels.Add derivedKernels
                stateToDerivedLookahead.Add derivedLookaheads
                vertex, mainLookaheads, derivedLookaheads, true
        if needDfs then
            for i = 0 to grammar.indexator.fullCount - 1 do
                if i <> grammar.indexator.eofIndex then
                    //check that at least one current kernel has new lookaheads
                    let mutable hasNewLookahead = false
                    for j = 0 to mainKernels.Length - 1 do
                        for (curSymbol, _) in curSymbolsAndNextPos mainKernels.[j] do
                            if curSymbol = i && not newMainLookaheads.[j].IsEmpty then hasNewLookahead <- true
                    for j = 0 to derivedKernels.Length - 1 do
                        for (curSymbol, _) in curSymbolsAndNextPos derivedKernels.[j] do
                            if curSymbol  = i && not newDerivedLookaheads.[j].IsEmpty then hasNewLookahead <- true
                    let destStates = new Dictionary<int, Set<int>>()
                    if hasNewLookahead then
                        for j = 0 to mainKernels.Length-1 do
                            for (curSymbol, nextPos) in curSymbolsAndNextPos mainKernels.[j] do
                                if curSymbol = i then
                                    let nextKernel = KernelInterpreterDFA.toKernel ((KernelInterpreterDFA.getProd mainKernels.[j]), nextPos)
                                    if not <| destStates.ContainsKey nextKernel then
                                        destStates.[nextKernel] <- newMainLookaheads.[j]
                                    else
                                        destStates.[nextKernel] <- Set.union destStates.[nextKernel] newMainLookaheads.[j]
                        for j = 0 to derivedKernels.Length-1 do
                            for (curSymbol, nextPos) in curSymbolsAndNextPos derivedKernels.[j] do
                                if curSymbol = i then
                                    let nextKernel = KernelInterpreterDFA.toKernel ((KernelInterpreterDFA.getProd derivedKernels.[j]), nextPos)
                                    if not <| destStates.ContainsKey nextKernel then
                                        destStates.[nextKernel] <- newDerivedLookaheads.[j]
                                    else
                                        destStates.[nextKernel] <- Set.union destStates.[nextKernel] newDerivedLookaheads.[j]
                        if destStates.Count <> 0 then
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
        let mainKernels,mainLookaheads,derivedKernels,derivedLookaheads = initKernelsAndLookAheads |> closure
        let setToStr = Set.map (sprintf "%d") >> String.concat ","
        let key = String.concat "|" (Array.map2 (fun x y -> sprintf "%d(%s)" x (setToStr y)) mainKernels mainLookaheads)
        //printfn "%s" key
        if kernelsToVertex.ContainsKey key then
            kernelsToVertex.[key]
        else
            let vertex = new Vertex<int,int>(nextIndex())
            //wasEdge.Add Set.empty
            vertices.Add vertex
            kernelsToVertex.[key] <- vertex
            stateToMainKernels.Add mainKernels
            stateToMainLookahead.Add mainLookaheads
            stateToDerivedKernels.Add derivedKernels
            stateToDerivedLookahead.Add derivedLookaheads
            for i = 0 to grammar.indexator.fullCount - 1 do
                if i <> grammar.indexator.eofIndex then
                    let destStates = new Dictionary<int, Set<int>>()
                    for j = 0 to mainKernels.Length-1 do
                        for (curSymbol, nextPos) in curSymbolsAndNextPos mainKernels.[j] do
                            if curSymbol = i && not mainLookaheads.[j].IsEmpty then
                                let nextKernel = KernelInterpreterDFA.toKernel ((KernelInterpreterDFA.getProd mainKernels.[j]), nextPos)
                                if not <| destStates.ContainsKey nextKernel then
                                    destStates.[nextKernel] <- mainLookaheads.[j]
                                else
                                    destStates.[nextKernel] <- Set.union destStates.[nextKernel] mainLookaheads.[j]
                    for j = 0 to derivedKernels.Length-1 do
                        for (curSymbol, nextPos) in curSymbolsAndNextPos derivedKernels.[j] do
                            if curSymbol = i && not derivedLookaheads.[j].IsEmpty then
                                let nextKernel = KernelInterpreterDFA.toKernel ((KernelInterpreterDFA.getProd derivedKernels.[j]), nextPos)
                                if not <| destStates.ContainsKey nextKernel then
                                    destStates.[nextKernel] <- derivedLookaheads.[j]
                                else
                                    destStates.[nextKernel] <- Set.union destStates.[nextKernel] derivedLookaheads.[j]
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

    let initKernels = grammar.rules.startPos grammar.startRule |> Array.map (fun x -> KernelInterpreter.toKernel(grammar.startRule, x))
    let initLookAhead = Set.ofSeq [grammar.indexator.eofIndex]
    let initKernelsAndLookAheads = Array.map (fun x -> (x, initLookAhead)) initKernels
    let threadFun = fun () ->
        initKernelsAndLookAheads
        |> match outTable with
            | LALR -> dfsLALR
            | LR -> dfsLR
        |> ignore
    let callStackSize = 10 * (1 <<< 20)
    let threadDfs = new Thread(threadFun, callStackSize)
    threadDfs.Start()
    threadDfs.Join()
    (*initKernelsAndLookAheads
        |> match outTable with
            | LALR -> dfsLALR
            | LR -> dfsLR
        |> ignore*)
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
    new StatesInterpreter(vertices.ToArray(), stateToMainKernels.ToArray(), stateToMainLookahead.ToArray(), stateToDerivedKernels.ToArray(), stateToDerivedLookahead.ToArray())