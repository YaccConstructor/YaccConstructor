
open Brahma.OpenCL
open OpenCL.Net
open Printf
open System
open System.Collections.Generic
open Alea.CUDA

open Util
open CYKMatrix





let printMatrix (matrix: ProbabilityMatrix.T) strLen searchLen = 
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    for i in [ 0..rowLength - 1 ] do
        for j in [ 0..colLength - 1 ] do
            let cell = Cell.create i j
            if i <= strLen && j <= strLen && j > i && j - i <= searchLen then 
                //                    if i <= strLen && j <= strLen && j > i then
                //                        printf "%b  " <| Probability.unwrap matrix.[i, j]
                printf "%.8f  " <| Probability.unwrap matrix.[cell]
            else 
                assert (Probability.isZero matrix.[cell])
                //                        printf "%.8f  " matrix.[i, j]
                printf "----------  "
        printfn ""
    printfn ""
    
let isAnswerValid (matrix: ProbabilityMatrix.T) strLen searchLen = 
    let rowLength = matrix.Nrow
    let colLength = matrix.Ncol
    if rowLength <> colLength || rowLength <> strLen + 1 then false
    else 
        let redundantCell (cell: Cell.T) = 
            cell.Row > strLen || cell.Column > strLen || cell.Column <= cell.Row 
            || cell.Column - cell.Row > searchLen
        [ 0..rowLength - 1 ]
        |> List.map (fun i -> [ 0..colLength - 1 ] |> List.map (fun j -> Cell.create i j))
        |> List.concat
        |> List.filter redundantCell
        |> List.forall (fun cell -> Probability.isZero matrix.[cell])





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

    //        let srl = new Dictionary<char, (NonTerminal * Probability.T) list>()
    //        ['a', [A, true]; 'b', [B, true]; 'c', [B, true]] 
    //        |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
    //        |> Seq.iter srl.Add
    let srl = new Dictionary<char, (NonTerminal * Probability.T) list>()
    [ 'a', [ A, 0.2; B, 0.1 ]
      'b', [ B, 0.4 ]
      'c', [ B, 0.3 ] ]
    |> List.map (fun (c, heads) -> c, rawHeadsToProbs heads)
    |> Seq.iter srl.Add

    let erl: NonTerminal list = []

    //    S -> A BB, 0.5
    //    S -> S S,  0.5
    //
    //    BB -> B B, 1.
    //
    //    A -> A A, 0.8
    //    A -> 'a', 0.2
    //
    //    B -> B B, 0.2
    //    B -> 'b', 0.4
    //    B -> 'c', 0.3
    //    B -> 'a', 0.1

    let rules = new RulesHolder(crl, srl, erl)

    
    

    let checkOneType task check taskType str searchLen = 
        let n = 100
        let doOne i =
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            task str searchLen |> ignore
            stopWatch.Stop()
//            GC.Collect()
            let time = stopWatch.Elapsed.TotalMilliseconds
            printfn "%f" time
            time
        let time = [1..n] |> List.map doOne |> List.min
        printfn "type: %s, str length: %i, search length: %i, time(ms): %f." taskType (String.length str) searchLen time
         






    
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


    let nvidiaOneThread, nvidiaParallel, nvidiaProvider = getGpuOptions nvidiaPlatformName
//    let intelOptions, intelProvider = getGpuOptions intelPlatformName
//    let amdOptions, amdProvider = getGpuOptions amdPlatformName
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
    
    let bestOption = (myAlg |> addCuda 128 cudaParallel |> addBrahma 32 nvidiaParallel |> addParallel 1)
    let toCheck1 = (myAlg |> addCuda 128 cudaParallel |> addBrahma 64 nvidiaParallel |> addParallel 1)
    let toCheck2 = (myAlg |> addCuda 128 cudaParallel |> add_1DBrahma 64 nvidiaParallel |> addFast 64 |> addParallel 1)
    let toCheck3 = (myAlg |> addCuda 64 cudaParallel |> addParallel 1)

    let checkTime str searchLen = 

        checkOneType 
            (fun str searchLen -> recognize toCheck2 str rules nonterminals S searchLen ) 
            (fun toCheck -> isAnswerValid toCheck) "my" str searchLen
            
//        checkOneType 
//            (fun str searchLen -> recognize bestOption str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "128, 64, parallel" str searchLen
//        checkOneType 
//            (fun str searchlen -> recognize (myAlg |> addCuda 128 cudaOneThread |> addBrahma 16 nvidiaOneThread) str rules nonterminals S searchlen ) 
//            (fun tocheck -> isAnswerValid tocheck) "128, 64, parallel" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize (myAlg |> addFast 64 |> addParallel 1) str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "fast 64, par" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize (myAlg |> addFast 64) str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "fast 64" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize myAlg str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "my, vanilla" str searchLen

//        checkOneType 
//            (fun str searchLen -> recognize (okhotinAlg |> addCuda 64 cudaParallel |> addParallel 16) str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, Gpu, par" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize (okhotinAlg |> addCuda 64 cudaOneThread) str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, Gpu, seq" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize (okhotinAlg |> addFast 64 |> addParallel 16) str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, fast, par" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize (okhotinAlg |> addFast 64) str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, fast 64" str searchLen
//        checkOneType 
//            (fun str searchLen -> recognize okhotinAlg str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "okhotin, vanilla" str searchLen


    let check str searchLen param1 param2 = 
        let toCheck1    = recognize param1 str rules nonterminals S searchLen 
        let toCheck2    = recognize param2 str rules nonterminals S searchLen 
        assert (isAnswerValid toCheck1 (String.length str) searchLen)
        assert (isAnswerValid toCheck2 (String.length str) searchLen)
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
                        
        printMatrix toCheck1 (String.length str) searchLen 
        printMatrix toCheck2 (String.length str) searchLen 

        if not sameAnswers
        then failwith "different answers"

    let checkMultiplicationNumber strLen param = 
        let str = (String.replicate strLen "a")
        let testParam = { param with Options.mode = Mode.Test }
        recognize testParam str rules nonterminals S strLen |> ignore
        printfn ""
    
//    let toCheckOptions = amdNewOptions
//    let toCheck = (myAlg |> addBrahma 8 nvidiaOneThread)
//    let toCheck = (myAlg |> add_1DBrahma 2 nvidiaOneThread)
//    let toCheck = (myAlg |> addCuda 2 cudaOneThread)
//    let toCheck = (myAlg |> addBrahma 2 nvidiaOneThread)
//    let toCheck = (myAlg |> addFast 4 |> addParallel 1 )

//    check "abb"      2
//    check "abb"      3    s
//    check "aaabbcc"  5
//    check "aaabb"    5
//    check "aaaaabbb" 6 okhotinAlg toCheck
//    check "aaaabbbbbb" 6 okhotinAlg toCheck
//    check "aaaabbbbbbbbbbb" 10
//    check "aaaabbbbbbbbbbbbbbb" 10
//    check "aaaabb" 6
//    check "aaaabb" 6
//    check "aaaabb" 5
//    check "aaaabb" 4
//    check "aaaabb" 3
//    check "aaaabb" 2
//    check "aaaabb" 1
//    check "aaaabb" 0
//    check (String.replicate 23 "abb") 69 okhotinAlg toCheck
//    check (String.replicate 350 "abb") 800 bestOption (myAlg |> addFast 64 |> addParallel 1) 

//    checkTime (String.replicate 200 "abb") 550
//    check (String.replicate 6 "abb") 18 okhotinAlg amdOptions
//    checkTime (String.replicate 120 "abb") 300
//    checkTime (String.replicate 200 "abb") 400

    checkMultiplicationNumber 63 myAlg


//    checkTime (String.replicate 700 "abb") 1600
    //        checkTime ((String.replicate 511 "abbb") + "abb") 50

    //        check "aabb"
    //        check "abb"    
    //        check "aaabbcc"
    //        check "baaabbcc"
    //        check "aaaabbcc"

//    System.Console.ReadLine() |> ignore
//    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    0
