// Recursive ascent parser generator

module Yard.Generators.RAGenerator.Internal

open System
open System.Collections.Generic
open System.IO

let (|KeyValue|) (kvp: KeyValuePair<_, _>) = kvp.Key, kvp.Value

type Identifier = string

type Terminal = string
type NonTerminal = string
type Symbol = Terminal of Terminal | NonTerminal of NonTerminal
type Symbols = Symbol list
      
type Prod = Prod of NonTerminal * Symbols

type ProcessedParserSpec = 
    { Terminals    : Terminal list
      NonTerminals : NonTerminal list
      Productions  : Prod list
      StartSymbol  : NonTerminal }

// Process LALR(1) grammars to tables

type ProductionIndex = int
type ProdictionDotIndex = int

/// Represent (ProductionIndex, ProductionDotIndex) as one integer 
type Item0 = uint32  

/// Part of the output of CompilerLalrParserSpec
type Action = 
  | Shift of int
  | Reduce of ProductionIndex
  | Accept
  | Error

/// LR(0) kernels
type Kernel = Set<Item0>

/// Indexes of LR(0) kernels in the KernelTable
type KernelIdx = int

/// Indexes in the TerminalTable and NonTerminalTable
type TerminalIndex = int
type NonTerminalIndex = int

type SymbolIdx = int
let PTerminal (i: TerminalIndex) : SymbolIdx = -i - 1
let PNonTerminal (i: NonTerminalIndex) : SymbolIdx = i
let (|PTerminal|PNonTerminal|) x = if x < 0 then PTerminal -(x + 1) else PNonTerminal x

type SymbolIdxes = SymbolIdx list

///   type KernelItemIndex = KernelItemIdx of KernelIdx * Item0
type KernelItemIndex = int64
let KernelItemIdx (i1, i2) = ((int64 i1) <<< 32) ||| int64 i2

///   type GotoItemIndex = GotoItemIdx of KernelIdx * SymbolIdx
type GotoItemIndex = uint64
let GotoItemIdx (i1: KernelIdx, i2: SymbolIdx) = (uint64 (uint32 i1) <<< 32) ||| uint64 (uint32 i2)
let (|GotoItemIdx|) (i64: uint64) = int32 ((i64 >>> 32) &&& 0xFFFFFFFFUL), int32 (i64 &&& 0xFFFFFFFFUL)

/// Create a work list and loop until it is exhausted, calling a worker function for
/// each element. Pass a function to queue additional work on the work list 
/// to the worker function
let ProcessWorkList start f =
    let work = ref start
    let queueWork = (fun x -> work := x :: !work)
    let rec loop() = 
        match !work with 
        | [] -> ()
        | x :: t -> 
            work := t
            f queueWork x
            loop()
    loop()

/// A general standard memoization utility (works only with 1 function)
let Memoize f = 
    let t = new Dictionary<_, _>(1000)
    fun x -> 
        let ok, v = t.TryGetValue x 
        if ok then v 
        else let res = f x 
             t.[x] <- res 
             res 

/// A standard utility to create a dictionary from a list of pairs
let CreateDictionary xs = 
    let dict = new Dictionary<_, _>()
    for x, y in xs do dict.Add(x, y)
    dict

/// Allocate indexes for each non-terminal
type NonTerminalTable(nonTerminals: NonTerminal list) = 
    let nonterminals = Array.ofList nonTerminals
    let idxsWithNonterminals = nonTerminals |> List.mapi (fun (i: NonTerminalIndex) n -> i, n)
    let nonterminalsWithIdxs = CreateDictionary [for i, x in idxsWithNonterminals -> x, i]
    let nonterminalIdxs = List.map fst idxsWithNonterminals

    member table.OfIndex i = nonterminals.[i]
    member table.ToIndex i = nonterminalsWithIdxs.[i]
    member table.Indexes = nonterminalIdxs

/// Allocate indexes for each terminal
type TerminalTable(terminalInfo: Terminal list) = 
    let idxsWithTerminals = terminalInfo |> List.mapi (fun i t -> i, t) 
    let terminalIdxs = List.map fst idxsWithTerminals
    let terminals = Array.ofList terminalInfo
    let terminalsWithIdxs = CreateDictionary [for i, x in idxsWithTerminals -> x, i]

    member table.OfIndex i = terminals.[i]
    member table.ToIndex i = terminalsWithIdxs.[i]
    member table.Indexes = terminalIdxs

/// Allocate indexes for each production
type ProductionTable(ntTab: NonTerminalTable, termTab: TerminalTable, nonTerminals: string list, prods: Prod list) =
    let idxsWithProds = List.mapi (fun i n -> i, n) prods

    let bodySymbolIdxs =  
        prods
        |> List.map (fun (Prod (_, syms)) -> 
              syms 
              |> Array.ofList  
              |> Array.map (function 
                            | Terminal t -> PTerminal (termTab.ToIndex t) 
                            | NonTerminal nt -> PNonTerminal (ntTab.ToIndex nt)))
        |> Array.ofList

    let headIdxs = 
        prods
        |> List.map (fun (Prod (nt, _)) -> ntTab.ToIndex nt)
        |> Array.ofList

    let headIdxsWithProdIdxs = 
        nonTerminals
        |> List.map (fun nt -> 
            (ntTab.ToIndex nt,
             idxsWithProds 
             |> List.choose (fun (i, Prod (nt2, syms)) -> 
                 if nt2 = nt then Some i else None)))
        |> CreateDictionary

    member this.HeadIndex i = headIdxs.[i]
    member this.BodyIndices i = bodySymbolIdxs.[i]
    member this.SymbolIdx i n = 
        let syms = this.BodyIndices i
        if n >= syms.Length then None else Some (syms.[n])
    member this.Productions = headIdxsWithProdIdxs

/// A mutable table mapping kernels to sets of lookahead tokens
type LookaheadTable() = 
    let t = new Dictionary<KernelItemIndex, Set<TerminalIndex>>()
    member this.Add(x, y) = 
        let prev = if t.ContainsKey x then t.[x] else Set.empty 
        t.[x] <- prev.Add y
    member this.Contains(x, y) = t.ContainsKey x && t.[x].Contains y
    member this.GetLookaheads (idx: KernelItemIndex) = 
        let ok, v = t.TryGetValue idx 
        if ok then v else Set.empty
    member this.Count = t |> Seq.fold (fun acc (KeyValue (_, v)) -> v.Count + acc) 0

/// A mutable table giving an index to each LR(0) kernel. Kernels are referred to only by index.
type KernelTable(kernels) =
    let kernelsArr = Array.ofList kernels 
    let kernelsAndIdxs = List.mapi (fun i x -> i, x) kernels
    let kernelIdxs = List.map fst kernelsAndIdxs
    let toIdxMap = Map.ofList [for i, x in kernelsAndIdxs -> x, i]
    member this.Indexes = kernelIdxs
    member this.Index kernel = toIdxMap.[kernel]
    member this.Kernel i = kernelsArr.[i]

/// Hold the results of computing the LALR(1) closure of an LR(0) kernel
type Closure1Table() = 
    let t = new Dictionary<Item0, HashSet<TerminalIndex>>()
    member this.Add(a, b) = 
        if not (t.ContainsKey a) then t.[a] <- new HashSet<_>(HashIdentity.Structural)
        t.[a].Add b
    member this.Count = t.Count
    member this.IEnumerable = (t :> seq<_>)
    member this.Contains(a, b) = t.ContainsKey a && t.[a].Contains b

/// A mutable table giving a lookahead set Set<Terminal> for each kernel. The terminals represent the
/// "spontaneous" items for the kernel.
type SpontaneousTable() = 
    let t = new Dictionary<KernelItemIndex, HashSet<TerminalIndex>>()
    member this.Add(a, b) = 
        if not (t.ContainsKey a) then t.[a] <- new HashSet<_>(HashIdentity.Structural)
        t.[a].Add b
    member this.Count = t.Count
    member this.IEnumerable = t :> seq<_>

/// A mutable table giving a Set<KernelItemIndex> for each kernel. The kernels represent the
/// "propagate" items for the kernel.
type PropagateTable() = 
    let t = new Dictionary<KernelItemIndex, HashSet<KernelItemIndex>>()
    member this.Add(a, b) = 
        if not (t.ContainsKey a) then t.[a] <- new HashSet<KernelItemIndex>(HashIdentity.Structural)
        t.[a].Add b
    member this.Item 
      with get a = 
        let ok, v = t.TryGetValue a 
        if ok then v :> seq<_> else Seq.empty
    member this.Count  = t.Count

/// Compile a pre-processed LALR parser spec to tables following the Dragon book algorithm
let generateInternal logfile (spec: ProcessedParserSpec) =
    // Augment the grammar 
    let fakeStartNonTerminal = "_start" + spec.StartSymbol
    let nonTerminals = fakeStartNonTerminal :: spec.NonTerminals
    let endOfInputTerminal = "$"
    let dummyLookahead = "#"
    let terminals = spec.Terminals @ [dummyLookahead; endOfInputTerminal]
    let prods = (Prod (fakeStartNonTerminal, [NonTerminal spec.StartSymbol])) :: spec.Productions

    // Build indexed tables 
    let ntTab = NonTerminalTable(nonTerminals)
    let termTab = TerminalTable(terminals)
    let prodTab = ProductionTable(ntTab, termTab, nonTerminals, prods)
    let dummyLookaheadIdx = termTab.ToIndex dummyLookahead
    let endOfInputTerminalIdx = termTab.ToIndex endOfInputTerminal

    // Compute the FIRST function
    printfn "Computing first function..."

    let computedFirstTable = 
        let seed = 
            Map.ofList
                [for term in termTab.Indexes do yield (PTerminal term, Set.singleton (Some term))
                 for nonTerm in ntTab.Indexes do 
                     yield 
                         (PNonTerminal nonTerm, 
                          List.foldBack 
                             (fun prodIdx acc -> 
                                 match prodTab.SymbolIdx prodIdx 0 with
                                 | None -> Set.add None acc 
                                 | Some _ -> acc) 
                             prodTab.Productions.[nonTerm] 
                             Set.empty)]
                 
        let add changed ss (x, y) = 
            let s = Map.find x ss
            if Set.contains y s then ss 
            else changed := true
                 Map.add x (Set.add y s) ss

        let oneRound (ss: Map<_, _>) = 
            let changed = ref false
            let frontier = 
                let res = ref []
                for nonTermX in ntTab.Indexes do 
                    for prodIdx in prodTab.Productions.[nonTermX] do
                        let rhs = Array.toList (prodTab.BodyIndices prodIdx)
                        let rec place l =
                            match l with
                            | yi :: t -> 
                                res := 
                                   List.choose 
                                       (function 
                                        | None -> None 
                                        | Some a -> Some (PNonTerminal nonTermX, Some a)) 
                                       (Set.toList ss.[yi])
                                   @ !res
                                if ss.[yi].Contains(None) then place t
                            | [] -> 
                                res := (PNonTerminal nonTermX, None) :: !res
                        place rhs
                !res
            let ss' = List.fold (add changed) ss frontier
            !changed, ss'

        let rec loop ss = 
            let changed, ss' = oneRound ss
            if changed then loop ss' else ss'
        loop seed 
            
      
    /// Compute the first set of the given sequence of non-terminals. If any of the non-terminals
    /// have an empty token in the first set then we have to iterate through those. 
    let ComputeFirstSetOfTokenList =
        Memoize (fun (str, term) -> 
            let acc = new System.Collections.Generic.List<_>()
            let rec add l = 
                match l with 
                | [] -> acc.Add term
                | sym :: moreSyms -> 
                    let firstSetOfSym = computedFirstTable.[sym]
                    firstSetOfSym |> Set.iter (function None -> () | Some v -> acc.Add v) 
                    if firstSetOfSym.Contains None then add moreSyms 
            add str
            Set.ofSeq acc)

    let mkItem0 (prodIdx, dotIdx) : Item0 = (uint32 prodIdx <<< 16) ||| uint32 dotIdx
    let prodIdx_of_item0 (item0: Item0) = int32 (item0 >>> 16)
    let dotIdx_of_item0 (item0: Item0) = int32 (item0 &&& 0xFFFFu)
    // (int, int) representation of LR(0) items 
    let prodIdx_to_item0 idx = mkItem0 (idx, 0)
    let ntIdx_of_item0 item0 = prodTab.HeadIndex (prodIdx_of_item0 item0)

    let lsyms_of_item0 item0 = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        let syms = prodTab.BodyIndices prodIdx
        if dotIdx = 0 then [||] else syms.[..dotIdx - 1] 

    let rsyms_of_item0 item0 = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        let syms = prodTab.BodyIndices prodIdx
        syms.[dotIdx..]

    let rsym_of_item0 item0 = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        prodTab.SymbolIdx prodIdx dotIdx

    let advance_of_item0 item0 = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        mkItem0 (prodIdx, dotIdx + 1)

    let IsStartItem item0 = ((ntTab.ToIndex fakeStartNonTerminal) = (ntIdx_of_item0 item0))
    let IsKernelItem item0 = (IsStartItem item0 || dotIdx_of_item0 item0 <> 0)

    (* Output functions *)

    let StringOfSym sym = match sym with PTerminal s -> "'" + termTab.OfIndex s + "'" | PNonTerminal s -> ntTab.OfIndex s

    let OutputSym os sym = fprintf os "%s" (StringOfSym sym)

    let OutputSyms os syms =
        fprintf os "%s" (String.Join(" ", Array.map StringOfSym syms))

    // Print items and other stuff 
    let OutputItem0 os item0 =
        fprintf os "    %s -> %a . %a" (ntTab.OfIndex (ntIdx_of_item0 item0)) OutputSyms (lsyms_of_item0 item0) OutputSyms (rsyms_of_item0 item0) 
        
    let OutputItem0Set os s = 
        Set.iter (fun item -> fprintf os "%a\n" OutputItem0 item) s

    let OutputAction os a = 
        match a with 
        | Shift n -> fprintf os "\tShift %d" n 
        | Reduce prodIdx -> fprintf os "\tReduce %s --> %a" (ntTab.OfIndex (prodTab.HeadIndex prodIdx)) OutputSyms (prodTab.BodyIndices prodIdx)
        | Error ->  fprintf os "\tError"
        | Accept -> fprintf os "\tAccept" 
    
    let OutputActions os m = 
        Array.iteri (fun i (action) -> 
            let term = termTab.OfIndex i in fprintf os "    Action on '%s': %a\n" term OutputAction action) m

    let OutputActionTable os t = 
        Array.iteri (fun i n -> fprintf os "State %d:\n%a\n" i OutputActions n) t
    
    let OutputGotos os m = 
        Array.iteri (fun ntIdx s -> 
            let nonterm = ntTab.OfIndex ntIdx in match s with Some st -> fprintf os "    Goto %s: %d\n" nonterm st | None -> ()) m
    
    let OutputCombined os m = 
        Array.iteri (fun i (a, b, c) -> 
            fprintf os "State %d:\n  items:\n%a\n  actions:\n%a\n  gotos:\n%a\n" i OutputItem0Set a OutputActions b OutputGotos c) m
    
    let OutputLalrTables os (states: Set<Item0>[], startState: int, 
                             actionTable: Action[][], gotoTable: int option [][]) = 
        let combined = 
            Array.ofList (List.map3 (fun x y z -> x, y, z) (Array.toList states) (Array.toList actionTable) (Array.toList gotoTable))
        fprintfn os "--------------------------"
        fprintfn os "States = "
        fprintfn os "%a" OutputCombined combined
        fprintfn os "StartState = %d" startState
        fprintfn os "--------------------------"

    let stateComment i items actions gotos =
        let sw = new StringWriter()
        fprintfn sw 
            "State %d:\n  items:\n%a\n  actions:\n%a\n  gotos:\n%a\n" i OutputItem0Set items OutputActions actions OutputGotos gotos
        let info = sw.ToString() 
        // FIX ".Length - 3 - 1 - 1" For some reason \n\r\n or smth like that appear at the end of string
        // & to remove blank line at the end as well
        let syms = "// " :: [for c in info.[..info.Length - 3 - 1 - 1] -> c.ToString()]
        List.foldBack (fun x acc -> if x = "\n" then "\n// " :: acc else x :: acc) syms []
        |> String.concat ""

    (* [End] Output functions *)

    // Close a set under epsilon moves
    let ComputeClosure0 iset = 
        /// A standard utility to compute a least fixed point of a set under a generative computation
        let LeastFixedPoint f set = 
            let acc = ref set
            ProcessWorkList (Set.toList set) (fun queueWork item ->
                  f item 
                  |> List.iter (fun i2 -> 
                      if not (Set.contains i2 !acc) then 
                          acc := Set.add i2 !acc
                          queueWork i2))
            !acc

        // Closure of LR(0) nonTerminals, items etc.
        let ComputeClosure0NonTerminal = 
            Memoize (fun nt -> 
                let seed = (List.foldBack (prodIdx_to_item0 >> Set.add) prodTab.Productions.[nt] Set.empty)
                LeastFixedPoint 
                    (fun item0 -> 
                         match rsym_of_item0 item0 with
                         | None -> []
                         | Some (PNonTerminal ntB) -> List.map prodIdx_to_item0 prodTab.Productions.[ntB]
                         | Some (PTerminal _) -> [])
                     seed)

        // Close a symbol under epsilon moves
        let ComputeClosure0Symbol rsym acc = 
            match rsym with
            | Some (PNonTerminal nt) -> Set.union (ComputeClosure0NonTerminal nt) acc
            | _ -> acc

        Set.fold (fun acc x -> ComputeClosure0Symbol (rsym_of_item0 x) acc) iset iset 

    // Right symbols after closing under epsilon moves
    let RelevantSymbolsOfKernel kernel = 
        let kernelClosure0 = ComputeClosure0 kernel
        Set.fold (fun acc x -> Option.fold (fun acc x -> Set.add x acc) acc (rsym_of_item0 x)) Set.empty kernelClosure0 

    // Goto set of a kernel of LR(0) nonTerminals, items etc.
    // Input is kernel, output is kernel
    let ComputeGotosOfKernel iset sym = 
        let isetClosure = ComputeClosure0 iset
        let acc = new System.Collections.Generic.List<_>(10)
        isetClosure |> Set.iter (fun item0 -> 
            match rsym_of_item0 item0 with   
            | Some sym2 when sym = sym2 -> acc.Add (advance_of_item0 item0) 
            | _ -> ())
        Set.ofSeq acc
    
    // Build the full set of LR(0) kernels 
    printfn "Building kernels..."
    let startItem = prodIdx_to_item0 0 // (!) assumption: start item always associates with the 1st production
    let startKernel = Set.singleton startItem
    let kernels = 
        let acc = ref Set.empty
        ProcessWorkList [startKernel] (fun addToWorkList kernel -> 
            if not ((!acc).Contains kernel) then
                acc := (!acc).Add kernel
                for csym in RelevantSymbolsOfKernel kernel do 
                    let gotoKernel = ComputeGotosOfKernel kernel csym 
                    assert (gotoKernel.Count > 0)
                    addToWorkList gotoKernel)
        !acc |> Seq.toList |> List.map (Set.filter IsKernelItem)
    
    printfn "Building kernel table..."
    // Give an index to each LR(0) kernel, and from now on refer to them only by index 
    let kernelTab = new KernelTable(kernels)
    let startKernelIdx = kernelTab.Index startKernel
    let startKernelItemIdx = KernelItemIdx (startKernelIdx, startItem)

    let outputKernelItemIdx os (kernelIdx, item0)  =
        fprintf os "Kernel %d, item %a" kernelIdx OutputItem0 item0

    /// A cached version of the "goto" computation on LR(0) kernels 
    let gotoKernel = 
        Memoize (fun (GotoItemIdx (kernelIdx, sym)) -> 
            let gset = ComputeGotosOfKernel (kernelTab.Kernel kernelIdx) sym
            if gset.IsEmpty then None else Some (kernelTab.Index gset))    

    // This is used to compute the closure of an LALR(1) kernel 
    let ComputeClosure1 iset = 
        let acc = new Closure1Table()
        ProcessWorkList iset (fun addToWorkList (item0, pretokens: Set<TerminalIndex>) ->
            pretokens |> Set.iter (fun pretoken -> 
                if not (acc.Contains(item0, pretoken)) then
                    acc.Add(item0, pretoken) |> ignore
                    let rsyms = rsyms_of_item0 item0 
                    if rsyms.Length > 0 then 
                        match rsyms.[0] with 
                        | PNonTerminal ntB -> 
                             let firstSet = ComputeFirstSetOfTokenList (Array.toList rsyms.[1..], pretoken)
                             for prodIdx in prodTab.Productions.[ntB] do
                                 addToWorkList (prodIdx_to_item0 prodIdx, firstSet)
                        | PTerminal _ -> ()))
        acc

    // Compute the "spontaneous" and "propagate" maps for each LR(0) kernelItem 
    printfn "Computing lookahead relations..."
        
    let spontaneous, propagate  =
        let closure1OfItem0WithDummy = 
            Memoize (fun item0 -> ComputeClosure1 [(item0, Set.ofList [dummyLookaheadIdx])])

        let spontaneous = new SpontaneousTable()
        let propagate = new PropagateTable()

        for kernelIdx in kernelTab.Indexes do
            let kernel = kernelTab.Kernel kernelIdx
            for item0 in kernel do  
                let item0Idx = KernelItemIdx(kernelIdx, item0)
                let jset = closure1OfItem0WithDummy item0
                for KeyValue (closureItem0, lookaheadTokens) in jset.IEnumerable do
                    match rsym_of_item0 closureItem0 with 
                    | None -> ()
                    | Some rsym ->
                         match gotoKernel (GotoItemIdx (kernelIdx, rsym)) with 
                         | None -> ()
                         | Some gotoKernelIdx ->
                              let gotoItem = advance_of_item0 closureItem0
                              let gotoItemIdx = KernelItemIdx (gotoKernelIdx, gotoItem)
                              for lookaheadToken in lookaheadTokens do
                                  if lookaheadToken = dummyLookaheadIdx 
                                  then propagate.Add(item0Idx, gotoItemIdx) |> ignore
                                  else spontaneous.Add(gotoItemIdx, lookaheadToken) |> ignore

        spontaneous, propagate

    printfn "Building lookahead table..."
    let lookaheadTable = 
        // Seed the table with the startKernelItems and the spontaneous info
        let initialWork =
            [yield (startKernelItemIdx, endOfInputTerminalIdx)
             for KeyValue (kernelItemIdx, lookaheads) in spontaneous.IEnumerable do
                 for lookahead in lookaheads do
                     yield (kernelItemIdx, lookahead)]

        let acc = new LookaheadTable()
        // Compute the closure
        ProcessWorkList initialWork (fun queueWork (kernelItemIdx, lookahead) ->
            acc.Add(kernelItemIdx, lookahead)
            for gotoKernelIdx in propagate.[kernelItemIdx] do
                if not (acc.Contains(gotoKernelIdx, lookahead)) then 
                    queueWork (gotoKernelIdx, lookahead))
        acc

    //printf  "built lookahead table, #lookaheads = %d\n" lookaheadTable.Count

    printfn "Building action table..."
    let shiftReduceConflicts = ref 0
    let actionTable = 
        // Now build the action tables. First a utility to merge the given action  
        // into the table
        let addResolve (arr: Action[]) kernelIdx termIdx actionNew = 
            let actionSoFar = arr.[termIdx]

            let winner = 
                match actionSoFar, actionNew with 
                | Shift _, Shift _ -> 
                   if actionSoFar <> actionNew then 
                       failwith "Internal error: shift/shift conflict"
                   actionSoFar
                | Reduce prodIdx1, Reduce prodIdx2 -> 
                    printfn "State %d: reduce/reduce conflict on %s\n" kernelIdx (termTab.OfIndex termIdx)
                    actionSoFar
                | (Shift  _ as shiftItem),  (Reduce _ as reduceItem)
                | (Reduce _ as reduceItem), (Shift  _ as shiftItem) -> 
                    printf "State %d: shift/reduce conflict on %s\n" kernelIdx (termTab.OfIndex termIdx)
                    incr shiftReduceConflicts
                    shiftItem
                | _ -> actionNew 
            arr.[termIdx] <- winner
          
        // This builds the action table for one state. 
        let ComputeActions kernelIdx = 
            let kernel = kernelTab.Kernel kernelIdx
            let arr = Array.create terminals.Length Error

            // Compute the LR(1) items based on lookaheads
            let items = 
                 [for item0 in kernel do
                    let kernelItemIdx = KernelItemIdx (kernelIdx, item0)
                    let lookaheads = lookaheadTable.GetLookaheads kernelItemIdx
                    yield (item0, lookaheads)]
                 |> ComputeClosure1

            for KeyValue (item0, lookaheads) in items.IEnumerable do
                let nonTermA = ntIdx_of_item0 item0
                match rsym_of_item0 item0 with 
                | Some (PTerminal termIdx) -> 
                    let action =
                      match gotoKernel (GotoItemIdx (kernelIdx, PTerminal termIdx)) with 
                      | None -> failwith "Action on terminal should have found a non-empty goto state"
                      | Some gkernelIdx -> Shift gkernelIdx
                    addResolve arr kernelIdx termIdx action
                | None ->
                    for lookahead in lookaheads do
                        if not (IsStartItem item0) then
                            let prodIdx = prodIdx_of_item0 item0
                            let action = Reduce prodIdx
                            addResolve arr kernelIdx lookahead action 
                        elif lookahead = endOfInputTerminalIdx then
                            let action = Accept
                            addResolve arr kernelIdx lookahead action 
                        else ()
                | _ -> ()

            arr

        let actionInfo = List.map ComputeActions kernelTab.Indexes
        Array.ofList actionInfo

    // The goto table is much simpler - it is based on LR(0) kernels alone. 

    printfn "Building goto table..."
    let gotoTable = 
         let gotos kernelIdx = 
            ntTab.Indexes
            |> List.map (fun nt -> gotoKernel (GotoItemIdx (kernelIdx, PNonTerminal nt))) 
            |> Array.ofList
         List.map gotos kernelTab.Indexes |> Array.ofList

    printfn "Returning tables."
    if !shiftReduceConflicts > 0 then printfn "%d shift/reduce conflicts" !shiftReduceConflicts

    let states = kernels |> Array.ofList

//    logfile (fun logStream -> 
//        printfn "Writing tables to log"
//        OutputLalrTables logStream (states, startKernelIdx, actionTable, gotoTable))

    let codeGenerate (states: Set<Item0>[]) (startState: int) 
                     (actionTable: Action[][]) (gotoTable: int option [][]) =
        let generateGotoHandlers (out: System.IO.StreamWriter) stateIdx state = 
            out.WriteLine "\t\twhile not !accepted && out = 0 do"
            out.WriteLine "\t\t\tmatch pre with"
            gotoTable.[stateIdx] 
            |> Array.iteri (fun ntIdx goto -> 
                match goto with
                | None -> ()
                | Some gotoStateNum -> 
                    let nonterm = ntTab.OfIndex ntIdx
                    out.WriteLine ("\t\t\t| Node (\"" + nonterm + "\", _) -> ")
                    if stateIdx = 0 then
                        out.WriteLine ("\t\t\t\tmatch state" + gotoStateNum.ToString() + " rest pre with")
                    else
                        out.WriteLine "\t\t\t\tlet curTreeList = match curTree with Children list -> list | _ -> [curTree]"
                        let rsymNts = 
                            Set.fold (fun acc x -> Option.fold (fun acc x -> Set.add x acc) acc (rsym_of_item0 x)) Set.empty state
                                |> Set.filter (fun x -> x >= 0)
                                |> Set.map (fun ntIdx -> ntTab.OfIndex ntIdx)
                        if rsymNts.Contains nonterm then
                            out.WriteLine ("\t\t\t\tmatch state" + gotoStateNum.ToString() + " rest (Children <| curTreeList @ [pre]) with")
                        else
                            out.WriteLine ("\t\t\t\tmatch state" + gotoStateNum.ToString() + " rest (Children [pre]) with")
                    out.WriteLine "\t\t\t\t| (r, p, o) -> rest <- r; pre <- p; out <- o")
            out.WriteLine "\t\t\t| _ -> failwith \"Parse error\""
            out.WriteLine "\t\t\tout <- out - 1"

        use out = new System.IO.StreamWriter("parser.fs")

        let baseCode = 
            "type Tree = \n\
             \t| Leaf of string\n\
             \t| Node of value: string * children: Tree\n\
             \t| Children of Tree list\n\
             \n\
             let accepted = ref false\n\
             \n\
             type outInt = int\n\
             let outCount tree : outInt = \n\
             \tmatch tree with\n\
             \t| Children x -> x.Length\n\
             \t| _ -> 1"
        
        out.WriteLine(baseCode)

        states |> Array.iteri (fun stateIdx state ->
            // Process a single function in rec ascent

//            out.WriteLine ("\n" + (stateComment stateIdx state actionTable.[stateIdx] gotoTable.[stateIdx]))

            let header = 
                (if stateIdx = 0 then "let rec " else "and ") + "state" + stateIdx.ToString() + " (input: string list) curTree = "
            out.WriteLine header
            out.WriteLine "\tmatch input.Head with"
            actionTable.[stateIdx]
            |> Array.iteri (fun actionIdx action -> 
                let curTerm = termTab.OfIndex actionIdx

                match action with 
                | Error -> ()
                | _ -> out.WriteLine ("\t| \"" + curTerm + "\" as sym -> ")

                match action with
                | Error -> () // handled by the failwith at the very bottom
                | Accept -> 
                    out.WriteLine "\t\tprintfn \"PASS\""
                    out.WriteLine "\t\taccepted := true"
                    out.WriteLine "\t\tinput, curTree, outCount curTree"
                | Reduce prodNum when (prodTab.BodyIndices prodNum).Length <> 0 -> 
                    out.WriteLine ("\t\tinput, (Node (\"" + (ntTab.OfIndex <| prodTab.HeadIndex prodNum) + "\", curTree)), outCount curTree")
                | Reduce prodNum -> // epsilon rules (.Length = 0)
                    out.WriteLine ("\t\tlet mutable rest, pre, out = input, (Node (\"" 
                                 + (ntTab.OfIndex <| prodTab.HeadIndex prodNum) 
                                 + "\", curTree)), outCount curTree")
                    generateGotoHandlers out stateIdx state
                    out.WriteLine "\t\trest, pre, out"
                | Shift shiftStateNum -> 
                    let curGotos = gotoTable.[stateIdx]
                    let realGotos = Array.filter (fun goto -> match goto with None -> false | _ -> true) curGotos
                    if realGotos.Length <> 0 then
                        out.WriteLine ("\t\tlet mutable rest, pre, out = state" + shiftStateNum.ToString() + " input.Tail (Leaf sym)")
                        out.WriteLine "\t\tout <- out - 1"
                        generateGotoHandlers out stateIdx state
                    else 
                        out.WriteLine "\t\tlet curTreeList = match curTree with Children list -> list | _ -> [curTree]"
                        out.WriteLine ("\t\tlet mutable rest, pre, out = state" + shiftStateNum.ToString() + " input.Tail (Children <| curTreeList @ [Leaf sym])")
                        out.WriteLine "\t\tout <- out - 1"
                    out.WriteLine "\t\trest, pre, out")
            out.WriteLine "\t| _ -> failwith \"Parse error\"")
    
//    codeGenerate states startKernelIdx actionTable gotoTable

    (states, startKernelIdx, actionTable, gotoTable)
    
  
// In-place testing
//let main = 
//    let example2 = 
////      let prods = [Prod ("S", [NonTerminal "C"]);
////                   Prod ("C", []);
////                   Prod ("C", [Terminal "d"])]
////
////      { Terminals    = ["c"; "d"]
////        NonTerminals = ["S"; "C"]
////        Productions  = prods
////        StartSymbol  = "S" }
//      
//      let prods = [Prod ("S", [NonTerminal "S"; Terminal "+"; NonTerminal "T"]);
//                   Prod ("S", [NonTerminal "T"]);
//                   Prod ("T", [NonTerminal "T"; Terminal "*"; NonTerminal "F"]);
//                   Prod ("T", [NonTerminal "F"]);
//                   Prod ("F", [Terminal "id"])]
//  
//      { Terminals    = ["+"; "*"; "id"]
//        NonTerminals = ["S"; "T"; "F"]
//        Productions  = prods
//        StartSymbol  = "S" }
//
////      let prods = [Prod ("A", [NonTerminal "C"; Terminal "x"; NonTerminal "A"]);
////                   Prod ("A", []);
////                   Prod ("B", [Terminal "x"; NonTerminal "C"; Terminal "y"]);
////                   Prod ("B", [Terminal "x"; NonTerminal "C"]);
////                   Prod ("C", [Terminal "x"; NonTerminal "B"; Terminal "x"]);
////                   Prod ("C", [Terminal "z"])]
////  
////      { Terminals    = ["x"; "y"; "z"]
////        NonTerminals = ["A"; "B"; "C"]
////        Productions  = prods
////        StartSymbol  = "A" }
//
//    let test ex = GenerateInternal (fun funPrint -> funPrint stdout) ex
//
//    let out = test example2
//
//    0