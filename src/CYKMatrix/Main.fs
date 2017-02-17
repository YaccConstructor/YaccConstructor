
open Brahma.OpenCL
open OpenCL.Net
open Printf
open System
open System.Collections.Generic
open Alea.CUDA

open Util
open CYKMatrix
open GraphParsing
open QuickGraph





let printMatrix (matrix: ProbabilityMatrix.T) inputLen searchLen = 
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    for i in [ 0..rowLength - 1 ] do
        for j in [ 0..colLength - 1 ] do
            let cell = Cell.create i j
            if i <= inputLen && j <= inputLen && j > i && j - i <= searchLen then 
                //                    if i <= inputLen && j <= inputLen && j > i then
                //                        printf "%b  " <| Probability.unwrap matrix.[i, j]
                printf "%.8f  " <| Probability.unwrap matrix.[cell]
            else 
                assert (Probability.isZero matrix.[cell])
                //                        printf "%.8f  " matrix.[i, j]
                printf "----------  "
        printfn ""
    printfn ""
    
let isAnswerValid (matrix: ProbabilityMatrix.T) inputLen searchLen = 
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    if rowLength <> colLength || rowLength <> inputLen + 1 then false
    else 
        let redundantCell (cell: Cell.T) = 
            cell.Row > inputLen || cell.Column > inputLen || cell.Column <= cell.Row 
            || cell.Column - cell.Row > searchLen
        [ 0..rowLength - 1 ]
        |> List.map (fun i -> [ 0..colLength - 1 ] |> List.map (fun j -> Cell.create i j))
        |> List.concat
        |> List.filter redundantCell
        |> List.forall (fun cell -> Probability.isZero matrix.[cell])


let graphParsingPrint (matrix: ProbabilityMatrix.T) =
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    for i in [ 0..rowLength - 1 ] do
        for j in [ 0..colLength - 1 ] do
            let cell = Cell.create i j
            printf "%.8f  " <| Probability.unwrap matrix.[cell]
        printfn ""
    printfn ""



let graphParsingTest1 =
    let graph = new AdjacencyGraph<int, TaggedEdge<int, int<AbstractAnalysis.Common.token>>>()
    graph.AddVertex(0) |> ignore
    graph.AddVertex(1) |> ignore

    graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(0, 1, 2*1<AbstractAnalysis.Common.token>)) |> ignore
    graph.AddEdge(new TaggedEdge<int, int<AbstractAnalysis.Common.token>>(1, 0, 2*1<AbstractAnalysis.Common.token>)) |> ignore
    let A = NonTerminal "A"
    let B = NonTerminal "B"
    let S = NonTerminal "S"
    let nonterminals = [| A; B; S |]

    let rawHeadsToProbs = List.map (fun (nt, prob) -> nt, Probability.create prob)

    let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
    [ (A, B), [ S, 1.0 ]
      (A, A), [ B, 1.0 ] ]
    |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
    |> Seq.iter crl.Add

    let srl = new Dictionary< int<AbstractAnalysis.Common.token>, (NonTerminal * Probability.T) list>()
    [ 2*1<AbstractAnalysis.Common.token>, [ A, 1.0 ] ]
    |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
    |> Seq.iter srl.Add

    let erl: NonTerminal list = []

    let rules = new RulesHolder(crl, srl, erl)

    let (recognizeMatrix, vertexToInt, multCount) = recognizeGraph graph naiveSquareMatrix rules nonterminals S
    
    printfn "Multiplacation count: %d" multCount
    graphParsingPrint recognizeMatrix


[<EntryPoint>]
let main args = 
    let A = NonTerminal "A"
    let B = NonTerminal "B"
    let BB = NonTerminal "BB"
    let S = NonTerminal "S"
    let nonterminals = [| A; B; S; BB |]

    let rawHeadsToProbs = List.map (fun (nt, prob) -> nt, Probability.create prob)
    //        let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
    //        [(A, BB), [S, true]; (S, S), [S, true]; (B, B), [BB, true; B, true]; (A, A), [A, true]] 
    //        |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
    //        |> Seq.iter crl.Add        
    let crl = new Dictionary<NonTerminal * NonTerminal, (NonTerminal * Probability.T) list>()
    [ (A, BB), [ S, 0.5 ]
      (S, S), [ S, 0.5 ]
      (B, B), [ BB, 1.; B, 0.2 ]
      (A, A), [ A, 0.8 ] ]
    |> List.map (fun (nts, heads) -> nts, rawHeadsToProbs heads)
    |> Seq.iter crl.Add

    //        let srl = new Dictionary<int, (NonTerminal * Probability.T) list>()
    //        [0, [A, true]; 1, [B, true]; 2, [B, true]] 
    //        |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
    //        |> Seq.iter srl.Add
    let srl = new Dictionary<int<AbstractAnalysis.Common.token>, (NonTerminal * Probability.T) list>()
    [ 0*1<AbstractAnalysis.Common.token>, [ A, 0.2; B, 0.1 ]
      1*1<AbstractAnalysis.Common.token>, [ B, 0.4 ]
      2*1<AbstractAnalysis.Common.token>, [ B, 0.3 ] ]
    |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
    |> Seq.iter srl.Add

    let erl: NonTerminal list = []

    //    S -> A BB, 0.5
    //    S -> S S,  0.5
    //
    //    BB -> B B, 1.
    //
    //    A -> A A, 0.8
    //    A -> 0, 0.2
    //
    //    B -> B B, 0.2
    //    B -> 1, 0.4
    //    B -> 2, 0.3
    //    B -> 0, 0.1

    let rules = new RulesHolder(crl, srl, erl)

    
    

    let checkOneType task check taskType (input:int<AbstractAnalysis.Common.token> list) searchLen = 
        let n = 100
        let doOne i =
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            task input searchLen |> ignore
            stopWatch.Stop()
//            GC.Collect()
            let time = stopWatch.Elapsed.TotalMilliseconds
            printfn "%f" time
            time
        let time = [1..n] |> List.map doOne |> List.min
        printfn "type: %s, input length: %i, search length: %i, time(ms): %f." taskType input.Length searchLen time
         






    
    let amdPlatformName = "AMD*"
    let intelPlatformName = "Intel*"
    let nvidiaPlatformName = "NVIDIA*"
    let defaultPlatformName = "*"

    let getGpuOptions platformName =
        let deviceType = DeviceType.Gpu
        //todo:
        let gpuOneThread = { PlatformName = platformName; DeviceType = deviceType; doParallelFlush = false }
        let gpuParallel = { gpuOneThread with doParallelFlush = true }
        let provider = 
            try 
                ComputeProvider.Create(platformName, deviceType)
            with ex -> failwith ex.Message
        gpuOneThread, gpuParallel, provider
            
    let printParams (provider: ComputeProvider) = 
        let globalMem = OpenCL.Net.DeviceInfo.GlobalMemSize
        let allocMem = OpenCL.Net.DeviceInfo.MaxMemAllocSize
        let globalCache = OpenCL.Net.DeviceInfo.GlobalMemCacheSize
        let localMem = OpenCL.Net.DeviceInfo.LocalMemSize
        let computeUnits = OpenCL.Net.DeviceInfo.MaxComputeUnits
        let WGSize = OpenCL.Net.DeviceInfo.MaxWorkGroupSize
        let WISizes = OpenCL.Net.DeviceInfo.MaxWorkItemSizes
        
        let getInfo infoType infoName div = 
            let info_, ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, infoType)
            let info = info_.CastTo<uint64>()
            let toMB x = int <| x / (uint64 div)
            printfn "%s: %d" infoName <| toMB info
        getInfo globalMem "global mem (MB)" (1 <<< 20)
        getInfo allocMem "alloc mem (MB)" (1 <<< 20)
        getInfo globalCache "global cache (MB)" (1 <<< 20)
        getInfo localMem "local mem (KB)" (1 <<< 10)
        getInfo computeUnits "comp units" 1
        getInfo WGSize "WG size (MB)" 1
        getInfo WISizes "WI sizes (B)" 1
                
//    printfn "AMD:"
//    printParams amdProvider
//    printfn "Intel:"
//    printParams intelProvider
//    printfn "NVIDIA:"
//    printParams nvidiaProvider
//    
//    0


//    let nvidiaOneThread, nvidiaParallel, nvidiaProvider = getGpuOptions nvidiaPlatformName
    let intelOptions, intelParallel, intelProvider = getGpuOptions intelPlatformName
//    let amdOneThreadOptions, amdParallel, amdProvider = getGpuOptions amdPlatformName
//    let defaultOptions, dafeultProvider = getGpuOptions defaultPlatformName
    let cudaOneThread: GPUCuda = { doParallelFlush = false }
    let cudaParallel: GPUCuda = { doParallelFlush = true }

    
    let myAlg = Options.empty Algorithm.Modified
    let okhotinAlg = Options.empty Algorithm.Okhotin
    let addCuda minms cuda (init: Options.T) = { init with Cuda = Some <| Options.createOne minms cuda } 
    let addBrahma minms brahma (init: Options.T) = { init with Brahma = Some <| Options.createOne minms brahma } 
    let add_1DBrahma minms brahma (init: Options.T) = { init with _1DBrahma = Some <| Options.createOne minms brahma } 
    let addParallel minms (init: Options.T) = { init with Parallel = Some <| Options.createOne minms () } 
    let addFast minms (init: Options.T) = { init with Fast = Some <| Options.createOne minms () } 
    
//    let bestOption = (myAlg |> addCuda 128 cudaParallel |> addBrahma 32 nvidiaParallel |> addParallel 1)
//    let toCheck1 = (myAlg |> addCuda 128 cudaParallel |> addBrahma 64 nvidiaParallel |> addParallel 1)
//    let toCheck2 = (myAlg |> addCuda 128 cudaParallel |> add_1DBrahma 64 nvidiaParallel |> addFast 64 |> addParallel 1)
    let toCheck3 = (myAlg |> addCuda 64 cudaParallel |> addParallel 1)

    let checkTime (input:int<AbstractAnalysis.Common.token> list) searchLen = 

        checkOneType 
            (fun input searchLen -> recognize toCheck3 input rules nonterminals S searchLen ) 
            (fun toCheck -> isAnswerValid toCheck) "my" input searchLen
            
//        checkOneType 
//            (fun input searchLen -> recognize bestOption input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "128, 64, parallel" input searchLen
//        checkOneType 
//            (fun input searchlen -> recognize (myAlg |> addCuda 128 cudaOneThread |> addBrahma 16 nvidiaOneThread) input rules nonterminals S searchlen ) 
//            (fun tocheck -> isAnswerValid tocheck) "128, 64, parallel" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize (myAlg |> addFast 64 |> addParallel 1) input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "fast 64, par" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize (myAlg |> addFast 64) input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "fast 64" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize myAlg input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "my, vanilla" input searchLen

//        checkOneType 
//            (fun input searchLen -> recognize (okhotinAlg |> addCuda 64 cudaParallel |> addParallel 16) input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, Gpu, par" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize (okhotinAlg |> addCuda 64 cudaOneThread) input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, Gpu, seq" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize (okhotinAlg |> addFast 64 |> addParallel 16) input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, fast, par" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize (okhotinAlg |> addFast 64) input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, fast 64" input searchLen
//        checkOneType 
//            (fun input searchLen -> recognize okhotinAlg input rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, vanilla" input searchLen


    let check input searchLen param1 param2 = 
        let toCheck1    = recognize param1 input rules nonterminals S searchLen 
        let toCheck2    = recognize param2 input rules nonterminals S searchLen 
        assert (isAnswerValid toCheck1 input.Length searchLen)
        assert (isAnswerValid toCheck2 input.Length searchLen)
        let printcellDiff (cell: Cell.T) =
            let v1 = Probability.unwrap toCheck1.[cell]
            let v2 = Probability.unwrap toCheck2.[cell]
            printfn "cell num %d, %d: %e, %e, %e" cell.Row cell.Column v1 v2 (abs <| v1 - v2)
        let sameCells cell = 
            //                (Probability.unwrap toCheck.[i, j]) = (Probability.unwrap toCheckBFS.[i, j])
            let mutable value = min (Probability.unwrap toCheck1.[cell]) (Probability.unwrap toCheck2.[cell])
            let mutable diff = abs <| (Probability.unwrap toCheck1.[cell]) - (Probability.unwrap toCheck2.[cell])
            if (diff * 10.) <= value
            then 
                true
            else 
                printcellDiff cell
                false
        let sameAnswers = 
            [ 0..toCheck1.Nrow - 1 ]
            |> Seq.forall (fun i -> (Seq.forall (fun j -> sameCells <| Cell.create i j) [ 0..toCheck1.Ncol - 1 ])) 
            
//        printcellDiff <| Cell.create 0 68
//        printcellDiff <| Cell.create 0 65
//        printcellDiff <| Cell.create 0 66
//        printcellDiff <| Cell.create 0 67
//        printcellDiff <| Cell.create 0 69

//        printcellDiff <| Cell.create 1 68
//        printcellDiff <| Cell.create 1 69
                        
//        printMatrix toCheck1 input.Length searchLen 
//        printMatrix toCheck2 input.Length searchLen 

        if not sameAnswers
        then failwith "different answers"

    let checkMultiplicationNumber inputLen param = 
        let input = List.replicate inputLen (0*1<AbstractAnalysis.Common.token>)
        let testParam = { param with Options.mode = Mode.Test }
        recognize testParam input rules nonterminals S inputLen |> ignore
        printfn ""
    
//    let toCheckOptions = amdNewOptions
//    let toCheck = (myAlg |> addBrahma 8 nvidiaOneThread)
//    let toCheck = (myAlg |> add_1DBrahma 2 nvidiaOneThread)
//    let toCheck = (myAlg |> addCuda 2 cudaOneThread)
//    let toCheck = (myAlg |> addBrahma 2 nvidiaOneThread)
    let toCheck = (myAlg)

//    check [0;1;1]      2
//    check [0;1;1]"      3    s
//    check [0;0;0;1;1;2;2]  5
//    check [0;0;0;1;1]    5
//    check [0;0;0;0;0;1;1;1] 6 okhotinAlg toCheck
//    check [0;0;0;0;1;1;1;1;1;1] 6 okhotinAlg toCheck
//    check [0;0;0;0;1;1;1;1;1;1;1;1;1;1;1] 10
//    check [0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1] 10
//    check [0;0;0;0;1;1] 6
//    check [0;0;0;0;1;1] 6
//    check [0;0;0;0;1;1] 5
//    check [0;0;0;0;1;1] 4
//    check [0;0;0;0;1;1] 3
//    check [0;0;0;0;1;1] 2
//    check [0;0;0;0;1;1] 1
//    check [0;0;0;0;1;1] 0
//    check (String.replicate 23 "abb") 69 okhotinAlg toCheck
//    check (String.replicate 350 "abb") 800 bestOption (myAlg |> addFast 64 |> addParallel 1) 

//    checkTime (String.replicate 200 "abb") 550
//    check (String.replicate 6 "abb") 18 okhotinAlg amdOptions
//    checkTime (String.replicate 120 "abb") 300
//    checkTime (String.replicate 200 "abb") 400

//    checkMultiplicationNumber 63 myAlg


//    checkTime (String.replicate 700 "abb") 1600
    //        checkTime ((String.replicate 511 "abbb") + "abb") 50

    //        check "aabb"
    //        check "abb"    
    //        check "aaabbcc"
    //        check "baaabbcc"
    //        check "aaaabbcc"

//    System.Console.ReadLine() |> ignore
//    System.Console.ReadLine() |> ignore

    graphParsingTest1

    System.Console.ReadLine() |> ignore
    0
