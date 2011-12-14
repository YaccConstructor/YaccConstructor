// (c) Microsoft Corporation 2005-2007.

module public Yard.Generators.GNESCCGenerator.LALR

open Yard.Generators.GNESCCGenerator
open System
open System.Collections.Generic
open Printf
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator.CommonTypes
open Yard.Generators.GNESCCGenerator.CommonTableFunctions

type Associativity = LeftAssoc | RightAssoc | NonAssoc

type Terminal = int
type NonTerminal = int
type Symbol = Terminal of Terminal | NonTerminal of NonTerminal | Dummy
type Symbols = Symbol list

//---------------------------------------------------------------------
// PreProcess Raw Parser Spec AST

type PrecedenceInfo = 
    | ExplicitPrec of Associativity * int 
    | NoPrecedence
      
type Production<'lbl when 'lbl : comparison> = 
    Production of NonTerminal * FAL.FA.FA<int,'lbl>

type ProcessedParserSpec<'lbl when 'lbl : comparison> = 
    { Terminals    : List<Terminal>
      NonTerminals : List<NonTerminal>
      Productions  : List<Production<Symbol*int>>
      StartSymbols : List<NonTerminal> 
    }

//-------------------------------------------------
// Process LALR(1) grammars to tables

//type ProductionIndex = int
type ProdictionDotIndex = int

/// Represent (ProductionIndex,ProdictionDotIndex) as one integer 
//type Item0 = uint32  
//
//let mkItem0 (prodIdx,dotIdx) : Item0 = (uint32 prodIdx <<< 16) ||| uint32 dotIdx
//let prodIdx_of_item0 (item0:Item0) = int32 (item0 >>> 16)
//let dotIdx_of_item0 (item0:Item0) = int32 (item0 &&& 0xFFFFu)

/// Part of the output of CompilerLalrParserSpec

  
/// LR(0) kernels
type Kernel = Set<Item0>

/// Indexes of LR(0) kernels in the KernelTable
type KernelIdx = int

/// Indexes in the TerminalTable and NonTerminalTable
type TerminalIndex = int
type NonTerminalIndex = int

/// Representation of Symbols.
/// Ideally would be declared as 
///    type SymbolIndex = PTerminal of TerminalIndex | PNonTerminal of NonTerminalIndex
/// but for performance reasons we embed as a simple integer (saves ~10%)
///
/// We use an active pattern to reverse the embedding.
type SymbolIndex = int
let PTerminal(i:TerminalIndex) : SymbolIndex = -i-1
let PNonTerminal(i:NonTerminalIndex) : SymbolIndex = i
let (|PTerminal|PNonTerminal|) x = if x < 0 then PTerminal (-(x+1)) else PNonTerminal x

type SymbolIndexes = SymbolIndex list

/// Indexes in the LookaheadTable, SpontaneousTable, PropagateTable
/// Embed in a single integer, since these are faster
/// keys for the dictionary hash tables
///
/// Logically:
///
///   type KernelItemIndex = KernelItemIdx of KernelIdx * Item0
type KernelItemIndex = int64
let KernelItemIdx (i1,i2) = ((int64 i1) <<< 32) ||| int64 i2


/// Indexes into the memoizing table for the Goto computations
/// Embed in a single integer, since these are faster
/// keys for the dictionary hash tables
///
/// Logically:
///
///   type GotoItemIndex = GotoItemIdx of KernelIdx * SymbolIndex
type GotoItemIndex = uint64
let GotoItemIdx (i1:KernelIdx,i2:SymbolIndex) = (uint64 (uint32 i1) <<< 32) ||| uint64 (uint32 i2)
let (|GotoItemIdx|) (i64:uint64) = int32 ((i64 >>> 32) &&& 0xFFFFFFFFUL), int32 (i64 &&& 0xFFFFFFFFUL)

/// Create a work list and loop until it is exhausted, calling a worker function for
/// each element. Pass a function to queue additional work on the work list 
/// to the worker function
let ProcessWorkList start f =
    let work = ref (start : 'a list)
    let queueWork = (fun x -> work := x :: !work)
    let rec loop() = 
        match !work with 
        | [] -> ()
        | x::t -> 
            work := t; 
            f queueWork x;
            loop()
    loop()

/// A standard utility to compute a least fixed point of a set under a generative computation
let LeastFixedPoint f set = 
    let acc = ref set
    ProcessWorkList (Set.toList set) (fun queueWork item ->
          f(item) |> List.iter (fun i2 -> if not (Set.contains i2 !acc) then (acc := Set.add i2 !acc; queueWork i2)) )
    !acc

/// A general standard memoization utility. Be sure to apply to only one (function) argument to build the
/// residue function!
let Memoize f = 
    let t = new Dictionary<_,_>(1000)
    fun x -> 
        let ok,v = t.TryGetValue(x) 
        if ok then v else let res = f x in t.[x] <- res; res 

/// A standard utility to create a dictionary from a list of pairs
let CreateDictionary xs = 
    let dict = new Dictionary<_,_>()
    for x,y in xs do dict.Add(x,y)
    dict

/// Allocate indexes for each non-terminal
type NonTerminalTable(nonTerminals:NonTerminal list) = 
    let nonterminalsWithIdxs = 
        //List.sort nonTerminals
        nonTerminals
        |> List.mapi (fun i n -> (i,n))
    let nonterminalIdxs = List.map fst nonterminalsWithIdxs
    let a = Array.ofList nonTerminals
    let b = CreateDictionary [ for i,x in nonterminalsWithIdxs -> x,i ];
    member table.OfIndex(i) = a.[i]
    member table.ToIndex(i) = b.[i]
    member table.Indexes = nonterminalIdxs

/// Allocate indexes for each terminal
type TerminalTable(terminals:Terminal list) =
    let terminalsWithIdxs = 
        //List.sort terminals
        terminals
        |> List.mapi (fun i t -> (i,t))
    let terminalIdxs = List.map fst terminalsWithIdxs
    let a = Array.ofList terminals    
    let c = CreateDictionary [ for i,x in terminalsWithIdxs -> x,i ]

    member table.OfIndex(i) = a.[i]    
    member table.ToIndex(i) = c.[i]
    member table.Indexes = terminalIdxs

let idx x y = x,y

/// Allocate indexes for each production
type ProductionTable(ntTab:NonTerminalTable, termTab:TerminalTable, nonTerminals:int list, prods: Production<_> list) =
    let prodsWithIdxs = List.mapi (fun i n -> (i,n)) prods

    let startIdx =
        prodsWithIdxs
        |> List.map (fun (i,Production(_,syms)) -> i,syms.Start)
        |> CreateDictionary

    let b = Array.ofList (List.map (fun (_,Production(nt,_)) -> ntTab.ToIndex nt) prodsWithIdxs)

    let productions = 
        nonTerminals
        |> List.map
            (fun nt -> 
                (ntTab.ToIndex nt
                ,List.choose 
                    (fun (i,Production(nt2,_)) -> if nt2=nt then Some i else None) 
                    prodsWithIdxs))
        |> CreateDictionary
    
    let productionIndexator f g = 
        prodsWithIdxs
        |> List.map 
            (function 
             | i,Production(nt,fa) ->                
                fa.Vertices
                |> List.ofSeq
                |> List.map 
                    (fun n ->
                       fa.OutEdges n
                       |> f fa n)
                |> g
                |> CreateDictionary
                |> idx i)
        |> CreateDictionary

    let symbols = 
        productionIndexator
            (fun fa n -> 
                Seq.map 
                    (fun eg -> 
                        match fst eg.Tag with 
                        | Terminal x     -> termTab.ToIndex x |> PTerminal |> Some
                        | NonTerminal x  -> ntTab.ToIndex x |> PNonTerminal |> Some
                        | Dummy          -> None)
                 >> List.ofSeq
                 >> (idx n))
            id

    let next =
        productionIndexator
            (fun fa n -> 
                Seq.map 
                    (fun (eg:QuickGraph.TaggedEdge<_,_>) -> 
                        (n 
                        ,match fst eg.Tag with 
                         | Terminal x     -> termTab.ToIndex x |> PTerminal |> Some
                         | NonTerminal x  -> ntTab.ToIndex x |> PNonTerminal |> Some 
                         | Dummy          -> None)
                        ,eg.Target))
            Seq.concat            

    let iterFA (fa:QuickGraph.BidirectionalGraph<_,_>) f edgeFilter s =
        let visited = ref []
        let rec inner s = 
            if List.exists ((=)s) !visited |> not
            then
                visited := s::!visited
                let oe = fa.OutEdges s 
                Seq.iter f oe
                Seq.filter edgeFilter oe
                |> Seq.map (fun e -> e.Target)  
                |> Seq.iter inner                                 
        inner s

    let iter (Production(nt,fa)) f edgeFilter = 
        iterFA fa f edgeFilter fa.Start.Value
                
    let reachable =
        [for i,Production(_,fa) in prodsWithIdxs ->
            let visited = new Dictionary<_,_>(fa.VertexCount)
            let resetVisited () = 
                for v in fa.Vertices do visited.[v] <- false
            let rec step v =
                if not visited.[v] 
                then
                    visited.[v] <- true
                    let subRes = new ResizeArray<_> ()
                    for eg in fa.OutEdges v do                                    
                        match fst eg.Tag with
                        | Terminal x     -> 
                            let elt = termTab.ToIndex x |> PTerminal
                            subRes.Add (elt,step eg.Target)
                        | NonTerminal x  -> 
                            let elt = ntTab.ToIndex x |> PNonTerminal
                            subRes.Add (elt,step eg.Target)
                        | Dummy          -> ()
                    [for (x,l) in subRes -> if List.isEmpty l then [[x]] else List.map (fun y -> x::y) l]                                    
                    |> List.concat
                else [[]]
            [for v in fa.Vertices -> resetVisited (); v, step v]
            |> CreateDictionary
            |> idx i]
        |> CreateDictionary

    member prodTab.StartID(i) = startIdx.[i].Value
    member prodTab.NonTerminal(i) = b.[i]    
    member prodTab.Symbols i n = symbols.[i].[n]
    member prodTab.Iter i f edgeFilter = iter (List.find (fun x -> fst x = i ) prodsWithIdxs |> snd) f edgeFilter
    member prodTab.Reachable i n = reachable.[i].[n]
    /// Get transition in production rule 'i', from vertex 'from' using edge, labeled with 'smb'
    member prodTab.Next i from smb = 
        next.[i].[from,smb]
    member prodTab.Productions = productions
    member prodTab.AllProds () = prodsWithIdxs

/// A mutable table maping kernels to sets of lookahead tokens
type LookaheadTable() = 
    let t = new Dictionary<KernelItemIndex,Set<TerminalIndex>>()
    member table.Add(x,y) = 
        let prev = if t.ContainsKey(x) then t.[x] else Set.empty 
        t.[x] <- prev.Add(y)
    member table.Contains(x,y) = t.ContainsKey(x) && t.[x].Contains(y)
    member table.GetLookaheads(idx:KernelItemIndex) = 
        let ok,v = t.TryGetValue(idx)  
        if ok then v else Set.empty
    member table.Count = t |> Seq.fold(fun acc (KeyValue(_,v)) -> v.Count+acc) 0

/// A mutable table giving an index to each LR(0) kernel. Kernels are referred to only by index.
type KernelTable(kernels) =
    // Give an index to each LR(0) kernel, and from now on refer to them only by index 
    // Also develop "kernelItemIdx" to refer to individual items within a kernel 
    let kernelsAndIdxs = List.mapi (fun i x -> (i,x)) kernels
    let kernelIdxs = List.map fst kernelsAndIdxs
    let toIdxMap = Map.ofList [ for i,x in kernelsAndIdxs -> x,i ]
    let ofIdxMap = Array.ofList kernels
    member t.Indexes = kernelIdxs
    member t.Index(kernel) = toIdxMap.[kernel]
    member t.Kernel(i) = ofIdxMap.[i]

/// Hold the results of cpmuting the LALR(1) closure of an LR(0) kernel
type Closure1Table() = 
    let t = new Dictionary<Item0, HashSet<TerminalIndex>>()
    member table.Add(a,b) = 
        if not (t.ContainsKey(a)) then t.[a] <- new HashSet<_>(HashIdentity.Structural)
        t.[a].Add(b)
    member table.Count  = t.Count
    member table.IEnumerable = (t :> seq<_>)
    member table.Contains(a,b) = t.ContainsKey(a) && t.[a].Contains(b)

/// A mutable table giving a lookahead set Set<Terminal> for each kernel. The terminals represent the
/// "spontaneous" items for the kernel. TODO: document this more w.r.t. the Dragon book.
type SpontaneousTable() = 
    let t = new Dictionary<KernelItemIndex,HashSet<TerminalIndex>>()
    member table.Add(a,b) = 
        if not (t.ContainsKey(a)) then t.[a] <- new HashSet<_>(HashIdentity.Structural)
        t.[a].Add(b)
    member table.Count  = t.Count
    member table.IEnumerable = (t :> seq<_>)

/// A mutable table giving a Set<KernelItemIndex> for each kernel. The kernels represent the
/// "propagate" items for the kernel. TODO: document this more w.r.t. the Dragon book.
type PropagateTable() = 
    let t = new Dictionary<KernelItemIndex,HashSet<KernelItemIndex>>()
    member table.Add(a,b) = 
        if not (t.ContainsKey(a)) then t.[a] <- new HashSet<KernelItemIndex>(HashIdentity.Structural)
        t.[a].Add(b)
    member table.Item 
      with get(a) = 
        let ok,v = t.TryGetValue(a) 
        if ok then v :> seq<_> else Seq.empty
    member table.Count  = t.Count


/// Compile a pre-processed LALR parser spec to tables following the Dragon book algorithm
let CompilerLalrParserSpec (spec : ProcessedParserSpec<_>) =
    let stopWatch = new System.Diagnostics.Stopwatch()
    let reportTime() = printfn "time: %A" stopWatch.Elapsed; stopWatch.Reset(); stopWatch.Start()
    stopWatch.Start()

    // Augment the grammar 
    let fakeStartNonTerminals = [Constants.gnesccStartRuleTag]
    let nonTerminals = spec.NonTerminals
    let endOfInputTerminal = Constants.gnesccEndStreamTag
    let dummyLookahead = Constants.gnesccDummyLookaheadTag
    let dummyPrec = NoPrecedence
    let terminals = spec.Terminals @ [(dummyLookahead); (endOfInputTerminal)]
    let prods = spec.Productions
    let startNonTerminalIdx_to_prodIdx (i:int) = i

    // Build indexed tables 
    let ntTab = NonTerminalTable(nonTerminals)
    let termTab = TerminalTable(terminals)
    let prodTab = ProductionTable(ntTab,termTab,nonTerminals,prods)
    let dummyLookaheadIdx = termTab.ToIndex dummyLookahead
    let endOfInputTerminalIdx = termTab.ToIndex endOfInputTerminal

    // Compute the FIRST function
    printf  "computing first function..."; stdout.Flush();

    let getSymbolIdx x = 
        match x with
        | Terminal x     -> termTab.ToIndex x |> PTerminal 
        | NonTerminal x  -> ntTab.ToIndex x |> PNonTerminal
        | Dummy          -> failwith "Dummy is not symbol"

    let computedFirstTable = 
        let seed = 
            Map.ofList
             [ for term in termTab.Indexes do yield (PTerminal(term),Set.singleton (Some term))
               for nonTerm in ntTab.Indexes do 
                  yield 
                    (PNonTerminal nonTerm, 
                     List.foldBack 
                       (fun prodIdx acc -> 
                            if (prodTab.Symbols prodIdx (prodTab.StartID prodIdx) |> List.exists (Option.isNone)) 
                            then Set.add None acc 
                            else acc) 
                       prodTab.Productions.[nonTerm]
                       Set.empty) ]
                 
        let add changed ss (x,y) = 
            let s = Map.find x ss
            if Set.contains y s then ss 
            else (changed := true; Map.add x (Set.add y s) ss)

        let oneRound (ss:Map<_,_>) = 
            let changed = ref false
            let frontier =
                let res = ref []
                for nonTermX in ntTab.Indexes do
                    for prodIdx in prodTab.Productions.[nonTermX] do
                        prodTab.Iter prodIdx
                            (fun e -> 
                                match fst e.Tag with
                                | Dummy         -> res := (PNonTerminal nonTermX,None) :: !res
                                | Terminal _
                                | NonTerminal _ as x -> 
                                    let y = getSymbolIdx x
                                    res := List.choose 
                                                (function None -> None | Some a -> Some (PNonTerminal nonTermX,Some a)) 
                                                (Set.toList ss.[y])
                                           @ !res)
                            (fun e -> 
                                match fst e.Tag with
                                | Dummy         -> false
                                | Terminal _ 
                                | NonTerminal _ as x -> 
                                    let y = getSymbolIdx x
                                    ss.[y].Contains(None))                        
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
        Memoize (fun (str,term) -> 
            let acc = new System.Collections.Generic.List<_>()
            let rec add l = 
                match l with 
                | [] -> acc.Add(term)
                | sym::moreSyms -> 
                    let firstSetOfSym = computedFirstTable.[sym]
                    firstSetOfSym |> Set.iter (function None -> () | Some v -> acc.Add(v)) 
                    if firstSetOfSym.Contains(None) then add moreSyms 
            add str;
            Set.ofSeq acc)
    
    // (int,int) representation of LR(0) items 
    let prodIdx_to_item0 idx = mkItem0(idx,prodTab.StartID idx)     
    let ntIdx_of_item0 item0 = prodTab.NonTerminal (prodIdx_of_item0 item0)

    let rsyms_of_item0 item0 = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        prodTab.Symbols prodIdx dotIdx

    let advance_of_item0 item0 smb = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        mkItem0(prodIdx,prodTab.Next prodIdx dotIdx (Some smb))

    let reachable item0 = 
        let prodIdx = prodIdx_of_item0 item0
        let dotIdx = dotIdx_of_item0 item0
        prodTab.Reachable prodIdx dotIdx

    let fakeStartNonTerminalsSet = Set.ofList (fakeStartNonTerminals |> List.map ntTab.ToIndex)

    let IsStartItem item0 = fakeStartNonTerminalsSet.Contains(ntIdx_of_item0 item0)
    let IsKernelItem item0 = (IsStartItem item0 || dotIdx_of_item0 item0 <> 0)

    // Closure of LR(0) nonTerminals, items etc 
    let ComputeClosure0NonTerminal = 
        Memoize (fun nt -> 
            let seed = (List.foldBack ((prodIdx_to_item0 >> fun x -> [x]) >> Set.add) prodTab.Productions.[nt] Set.empty)
            LeastFixedPoint 
                (fun item0 -> 
                    List.map 
                        (function
                           | None -> []
                           | Some(PNonTerminal ntB) ->  List.map prodIdx_to_item0 prodTab.Productions.[ntB]
                           | Some(PTerminal _) -> [])
                        (List.map rsyms_of_item0 item0
                         |> List.concat))
                seed)

    // Close a symbol under epsilon moves
    let ComputeClosure0Symbol rsym acc = 
        match rsym with
        | Some (PNonTerminal nt) -> (ComputeClosure0NonTerminal nt) |> List.concat |> Set.ofList |> (+)  acc
        | _ -> acc

    // Close a set under epsilon moves
    let ComputeClosure0 iset =
        Set.fold
            (fun acc x ->
                rsyms_of_item0 x
                |> Seq.fold (fun acc x -> ComputeClosure0Symbol x acc) acc)
            iset iset

    // Right symbols after closing under epsilon moves
    let RelevantSymbolsOfKernel kernel =
        let kernelClosure0 = ComputeClosure0 kernel
        Set.fold 
            (fun acc x -> List.map (Option.fold (fun acc x -> Set.add x acc) acc)  (rsyms_of_item0 x) |> Set.unionMany)
            Set.empty kernelClosure0 

    // Goto set of a kernel of LR(0) nonTerminals, items etc 
    // Input is kernel, output is kernel
    let ComputeGotosOfKernel iset sym = 
        let isetClosure = ComputeClosure0 iset
        let acc = new System.Collections.Generic.List<_>(10)
        isetClosure 
        |> Set.iter (fun item0 -> 
              rsyms_of_item0 item0 
              |> List.iter
                (function 
                 | Some sym2 when sym = sym2 -> acc.Add(advance_of_item0 item0 sym) 
                 | _ -> ()))
#if DEBUG 
        printfn "ComputeGotosOfKernel"
        printf "iset = "
        Seq.iter (printf "%A; ") iset
        printfn ""
        printfn "symbol = %i" sym        
        printf "result = "
        Seq.iter (printf "%A; ") acc
        printfn ""
        printfn ""
#endif        
        Set.ofSeq acc

    let i0Data x = 
        let pIdx = prodIdx_of_item0 x
        let dotIdx = dotIdx_of_item0 x
        pIdx,dotIdx
            
    let kernelToItemsMap = ref []
    // Build the full set of LR(0) kernels 
    reportTime(); printf "building kernels..."; stdout.Flush();
    let startItems = List.mapi (fun i _ -> prodIdx_to_item0 (startNonTerminalIdx_to_prodIdx i)) fakeStartNonTerminals
    let startKernels = List.map Set.singleton startItems
    let kernels = 

        /// We use a set-of-sets here. F# sets support structural comparison but at the time of writing
        /// did not structural hashing. 
        let acc = ref Set.empty
        ProcessWorkList startKernels (fun addToWorkList kernel -> 
            if not ((!acc).Contains(kernel)) then
                acc := (!acc).Add(kernel);
                for csym in RelevantSymbolsOfKernel kernel do 
                    let gotoKernel = ComputeGotosOfKernel kernel csym 
                    assert (gotoKernel.Count > 0)
                    addToWorkList gotoKernel )
        let l = !acc |> Seq.toList |> List.map (Set.map i0Data)
        let kernels = !acc |> Seq.toList |> List.map (Set.filter IsKernelItem)
        kernelToItemsMap := List.zip (kernels)(!acc |> Seq.toList |> List.map ComputeClosure0)
        kernels
    
    reportTime(); printf "building kernel table..."; stdout.Flush();
    // Give an index to each LR(0) kernel, and from now on refer to them only by index     
    let kernelTab = new KernelTable(kernels)
    let kernelToItemIdx = 
        !kernelToItemsMap
        |> List.map (fun (x,y) -> (kernelTab.Index x ,y))
        |> List.sortBy fst
        |> List.map snd
        |> Array.ofList
    let startKernelIdxs = List.map kernelTab.Index startKernels
    let startKernelItemIdxs = List.map2 (fun a b -> KernelItemIdx(a,b)) startKernelIdxs startItems

//    let outputKernelItemIdx os (kernelIdx,item0)  =
//        fprintf os "kernel %d, item %a" kernelIdx OutputItem0 item0

    /// A cached version of the "goto" computation on LR(0) kernels 
    let gotoKernel = 
        Memoize (fun (GotoItemIdx(kernelIdx,sym)) ->
            let gset = ComputeGotosOfKernel (kernelTab.Kernel kernelIdx) sym
            if gset.IsEmpty then None else Some (kernelTab.Index gset))

    /// Iterate (iset,sym) pairs such that (gotoKernel kernelIdx sym) is not empty
//    let IterateGotosOfKernel kernelIdx f =
//        for sym in RelevantSymbolsOfKernel (kernelTab.Kernel kernelIdx) do 
//            match gotoKernel (GotoItemIdx(kernelIdx,sym)) with 
//            | None -> ()
//            | Some k -> f sym k
    

    // This is used to compute the closure of an LALR(1) kernel 
    //
    // For each item [A --> X.BY, a] in I
    //   For each production B -> g in G'
    //     For each terminal b in FIRST(Ya)
    //        such that [B --> .g, b] is not in I do
    //            add [B --> .g, b] to I
    
    let ComputeClosure1 iset = 
        let acc = new Closure1Table()
        ProcessWorkList iset (fun addToWorkList (item0,pretokens:Set<TerminalIndex>) ->
            pretokens |> Set.iter (fun pretoken -> 
                if not (acc.Contains(item0,pretoken)) then
                    acc.Add(item0,pretoken) |> ignore
                    let rsyms = rsyms_of_item0 item0                    
                    if rsyms.Length > 0 then
                        rsyms
                        |> Seq.iter 
                            (function
                             | Some(PNonTerminal ntB) -> 
                                 let firstSet = 
                                    let r = 
                                        reachable item0
                                        |> List.map List.tail
                                    r
                                    |> List.map (fun lst -> ComputeFirstSetOfTokenList (lst,pretoken))
                                    |> Set.unionMany
                                    |> Set.ofSeq
                                 //let firstSet = ComputeFirstSetOfTokenList (reachable item0,pretoken)
                                 for prodIdx in prodTab.Productions.[ntB] do
                                     addToWorkList (prodIdx_to_item0 prodIdx,firstSet)
                             | Some(PTerminal _)|None -> ())))
        acc

    // Compute the "spontaneous" and "propagate" maps for each LR(0) kernelItem 
    //
    // Input: The kernal K of a set of LR(0) items I and a grammar symbol X
    //
    // Output: The lookaheads generated spontaneously by items in I for kernel items 
    // in goto(I,X) and the items I from which lookaheads are propagated to kernel
    // items in goto(I,X)
    //
    // Method
    //   1. Construct LR(0) kernel items (done - above)
    //   2. 
    // TODO: this is very, very slow. 
    //
    // PLAN TO OPTIMIZE THIS;
    //   - Clarify and comment what's going on here
    //   - verify if we really have to do these enormouos closure computations
    //   - assess if it's possible to use the symbol we're looking for to help trim the jset
    
    reportTime(); printf "computing lookahead relations..."; stdout.Flush();

        
    let spontaneous, propagate  =
        let closure1OfItem0WithDummy = 
            Memoize (fun item0 -> ComputeClosure1 [(item0,Set.ofList [dummyLookaheadIdx])])

        let spontaneous = new SpontaneousTable()
        let propagate = new PropagateTable()
        let count = ref 0 

        for kernelIdx in kernelTab.Indexes do
            printf  "."; stdout.Flush();
            //printf  "kernelIdx = %d\n" kernelIdx; stdout.Flush();
            let kernel = kernelTab.Kernel(kernelIdx)
            for item0 in kernel do  
                let item0Idx = KernelItemIdx(kernelIdx,item0)
                let jset = closure1OfItem0WithDummy item0
                //printf  "#jset = %d\n" jset.Count; stdout.Flush();
                for (KeyValue(closureItem0, lookaheadTokens)) in jset.IEnumerable do
                    incr count
                    rsyms_of_item0 closureItem0
                    |> List.iter
                        (function
                         | None -> ()
                         | Some rsym ->
                             match gotoKernel (GotoItemIdx(kernelIdx,rsym)) with 
                             | None -> ()
                             | Some gotoKernelIdx ->
                                  let gotoItem = advance_of_item0 closureItem0 rsym
                                  let gotoItemIdx = KernelItemIdx(gotoKernelIdx,gotoItem)
                                  for lookaheadToken in lookaheadTokens do
                                      if lookaheadToken = dummyLookaheadIdx 
                                      then propagate.Add(item0Idx, gotoItemIdx) |> ignore
                                      else spontaneous.Add(gotoItemIdx, lookaheadToken) |> ignore)                        


        //printfn "#kernelIdxs = %d, count = %d" kernelTab.Indexes.Length !count
        spontaneous, propagate
   
    //printfn "#spontaneous = %d, #propagate = %d" spontaneous.Count propagate.Count; stdout.Flush();
   
    //exit 0;
    // Repeatedly use the "spontaneous" and "propagate" maps to build the full set 
    // of lookaheads for each LR(0) kernelItem.   
    reportTime(); printf  "building lookahead table..."; stdout.Flush();
    let lookaheadTable = 

        // Seed the table with the startKernelItems and the spontaneous info
        let initialWork =
            [ for idx in startKernelItemIdxs do
                  yield (idx,endOfInputTerminalIdx)
              for (KeyValue(kernelItemIdx,lookaheads)) in spontaneous.IEnumerable do
                  for lookahead in lookaheads do
                      yield (kernelItemIdx,lookahead) ]

        let acc = new LookaheadTable()
        // Compute the closure
        ProcessWorkList 
            initialWork
            (fun queueWork (kernelItemIdx,lookahead) ->
                acc.Add(kernelItemIdx,lookahead)
                for gotoKernelIdx in propagate.[kernelItemIdx] do
                    if not (acc.Contains(gotoKernelIdx,lookahead)) then 
                        queueWork(gotoKernelIdx,lookahead))
        acc
#if DEBUG
    printf  "built lookahead table, #lookaheads = %d\n" lookaheadTable.Count; stdout.Flush();
#endif

//    let OutputAction os m = 
//        match m with 
//        | Shift n -> fprintf os "  shift %d" n 
//        | Reduce prodIdx ->  fprintf os "  reduce %A --> %a" (ntTab.OfIndex (prodTab.NonTerminal prodIdx)) OutputSyms (prodTab.Symbols prodIdx)
//        | Error ->  fprintf os "  error"
//        | Accept -> fprintf os "  accept"

    reportTime(); printf "building action table..."; stdout.Flush();
    let shiftReduceConflicts = ref 0
    let reduceReduceConflicts = ref 0
    
    let actionTable(*, immediateActionTable*) = 

        // Now build the action tables. First a utility to merge the given action  
        // into the table, taking into account precedences etc. and reporting errors. 
        let addResolvingPrecedence (arr: _[]) kernelIdx termIdx (precNew, actionNew) = 
            //printf "DEBUG: state %d: adding action for %A, precNew = %a, actionNew = %a\n" kernelIdx (termTab.OfIndex termIdx) outputPrec precNew OutputAction actionNew; 
            // We add in order of precedence - however the precedences may be the same, and we give warnings when rpecedence resolution is based on implicit file orderings 

            //let (precSoFar, actionSoFar) as itemSoFar = arr.[termIdx]

            //printf "DEBUG: state %d: adding action for %A, precNew = %a, precSoFar = %a, actionSoFar = %a\n" kernelIdx (termTab.OfIndex termIdx) outputPrec precNew outputPrec precSoFar OutputAction actionSoFar; 
            // if compare_prec precSoFar precNew = -1 then failwith "addResolvingPrecedence"; 

            let itemNew = (precNew, actionNew) 
            let winner =
                match arr.[termIdx] with
                | [(NoPrecedence,Error)] -> [itemNew]
                | x -> itemNew :: x
//                match itemSoFar,itemNew with 
//                | (_,Shift _),(_, Shift _) -> 
//                   if actionSoFar <> actionNew then 
//                      printf "internal error in fsyacc: shift/shift conflict";
//                   itemSoFar
//
//                | (((precShift,Shift _) as shiftItem), 
//                   ((precReduce,Reduce _) as reduceItem))
//                | (((precReduce,Reduce _) as reduceItem), 
//                   ((precShift,Shift _) as shiftItem)) -> 
//                    match precReduce, precShift with 
//                    | (ExplicitPrec (_,p1), ExplicitPrec(assocNew,p2)) -> 
//                      if p1 < p2 then shiftItem
//                      elif p1 > p2 then reduceItem
//                      else
//                        match precShift with 
//                        | ExplicitPrec(LeftAssoc,_) ->  reduceItem
//                        | ExplicitPrec(RightAssoc,_) -> shiftItem
//                        | _ ->
//                           printf "state %d: shift/reduce error on %s\n" kernelIdx ((termTab.OfIndex termIdx).ToString()); 
//                           incr shiftReduceConflicts;
//                           shiftItem
//                    | _ ->
//                       printf "state %d: shift/reduce error on %s\n" kernelIdx ((termTab.OfIndex termIdx).ToString()); 
//                       incr shiftReduceConflicts;
//                       shiftItem
//                | ((_, Reduce prodIdx1),(_, Reduce prodIdx2)) -> 
//                   printf "state %d: reduce/reduce error on %s\n" kernelIdx ((termTab.OfIndex termIdx).ToString()); 
//                   incr reduceReduceConflicts;
//                   if prodIdx1 < prodIdx2 then itemSoFar else itemNew
//                | _ -> itemNew 
            arr.[termIdx] <- winner

          
        // This build the action table for one state. 
        let ComputeActions kernelIdx = 
            let kernel = kernelTab.Kernel kernelIdx
            let arr = Array.create terminals.Length ([NoPrecedence,Error])

#if DEBUG
            printf  "building lookahead table LR(1) items for kernelIdx %d\n" kernelIdx; stdout.Flush();
#endif

            // Compute the LR(1) items based on lookaheads
            let items = 
                 [ for item0 in kernel do
                     let kernelItemIdx = KernelItemIdx(kernelIdx,item0)
                     let lookaheads = lookaheadTable.GetLookaheads(kernelItemIdx)
                     yield (item0,lookaheads) ]
                 |> ComputeClosure1
                             
            for (KeyValue(item0,lookaheads)) in items.IEnumerable do
                //let nonTermA = ntIdx_of_item0 item0
                let rsyms = rsyms_of_item0 item0
                rsyms
                |> Seq.iter
                   (function 
                    | Some (PTerminal termIdx) -> 
                        let action =
                            match gotoKernel (GotoItemIdx(kernelIdx,PTerminal termIdx)) with 
                            | None -> failwith "action on terminal should have found a non-empty goto state"
                            | Some gkernelItemIdx -> Shift gkernelItemIdx
                        let prec = NoPrecedence
                        addResolvingPrecedence arr kernelIdx termIdx (prec, action) 
                    | None ->
                        for lookahead in lookaheads do
                            if not (IsStartItem(item0)) then
                                let prodIdx = prodIdx_of_item0 item0
                                let prec = NoPrecedence
                                let action = (prec, Reduce prodIdx)
                                addResolvingPrecedence arr kernelIdx lookahead action 
                            elif lookahead = endOfInputTerminalIdx then
                                let prec = NoPrecedence
                                let action = (prec,Accept)
                                addResolvingPrecedence arr kernelIdx lookahead action 
                            else ()
                    | _ -> ())                

            // If there is a single item A -> B C . and no Shift or Accept actions (i.e. only Error or Reduce, so the choice of terminal 
            // cannot affect what we do) then we emit an immediate reduce action for the rule corresponding to that item 
            // Also do the same for Accept rules. 
            //let closure = (ComputeClosure0 kernel)

            (*let immediateAction =
                match Set.toList closure with
                | [item0] ->
                    match (rsym_of_item0 item0) with 
                    | None when (let reduceOrErrorAction = function Error | Reduce _ -> true | Shift _ | Accept -> false
                                 termTab.Indexes |> List.forall(fun terminalIdx -> reduceOrErrorAction (snd(arr.[terminalIdx]))))
                        -> Some (Reduce (prodIdx_of_item0 item0))

                    | None when (let acceptOrErrorAction = function Error | Accept -> true | Shift _ | Reduce _ -> false
                                 List.forall (fun terminalIdx -> acceptOrErrorAction (snd(arr.[terminalIdx]))) termTab.Indexes)
                        -> Some Accept

                    | _ -> None
                | _ -> None*)

            // A -> B C . rules give rise to reductions in favour of errors 
            for item0 in ComputeClosure0 kernel do
                let prec = NoPrecedence
                rsyms_of_item0 item0
                |> Seq.iter
                   (function
                    | None ->
                        for terminalIdx in termTab.Indexes do 
                            if arr.[terminalIdx].Length = 1 && snd(arr.[terminalIdx].Head) = Error then 
                                let prodIdx = prodIdx_of_item0 item0
                                let action = (prec, (if IsStartItem(item0) then Accept else Reduce prodIdx))
                                addResolvingPrecedence arr kernelIdx terminalIdx action
                    | _  -> ())

            arr//,immediateAction

        let actionInfo = List.map ComputeActions kernelTab.Indexes
        Array.ofList actionInfo
        //Array.ofList (List.map snd actionInfo)


    // The goto table is much simpler - it is based on LR(0) kernels alone. 

    reportTime(); printf  "building goto table..."; stdout.Flush();
    let gotoTable = 
         let gotos kernelIdx = Array.ofList (List.map (fun nt -> gotoKernel (GotoItemIdx(kernelIdx,PNonTerminal nt))) ntTab.Indexes)
         Array.ofList (List.map gotos kernelTab.Indexes)

    reportTime(); printfn  "returning tables."; stdout.Flush();
    if !shiftReduceConflicts > 0 then printfn  "%d shift/reduce conflicts" !shiftReduceConflicts; stdout.Flush();
    if !reduceReduceConflicts > 0 then printfn  "%d reduce/reduce conflicts" !reduceReduceConflicts; stdout.Flush();

    /// The final results
    let states = kernels |> Array.ofList    

    let symbolIdx = 
        let idx = [|0.. termTab.Indexes.Length + ntTab.Indexes.Length |]
        List.iter (fun x -> idx.[termTab.OfIndex(x)] <- x) termTab.Indexes
        List.iter (fun x -> idx.[ntTab.OfIndex(x)] <- x) ntTab.Indexes
        idx

    let states = states
    (prodTab, states, startKernelIdxs,
     actionTable, gotoTable,
     (termTab.ToIndex endOfInputTerminal),
      nonTerminals, symbolIdx, kernelToItemIdx, ntTab)