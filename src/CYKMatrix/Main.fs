open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.Helpers
open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open OpenCL.Net
open Printf
open System
open System.Collections.Generic

open Util
open CYKMatrix

//    open OpenCL.Net.Extensions
[<EntryPoint>]
let main args = 

    let amdPlatformName = "AMD*"
    let intelPlatformName = "Intel*"
    let nvidiaPlatformName = "NVIDIA*"
    let defaultPlatformName = "*"

    let getGpuOptions platformName minMatrixSize =
        let deviceType = DeviceType.Gpu
        let gpuOneThread = 
            { PlatformName = platformName; DeviceType = deviceType; MinMatrixSize = minMatrixSize; doParallelFlush = false }
        let gpuParallel = { gpuOneThread with doParallelFlush = true }
        let multOneThread = { Options.empty Algorithm.Modified with GPU = Some gpuOneThread }
        let multParallel = { Options.empty Algorithm.Modified with GPU = Some gpuParallel }
        let provider = 
            try 
                ComputeProvider.Create(platformName, deviceType)
            with ex -> failwith ex.Message
        multOneThread, multParallel, provider
        
    let nvidiaOneThread, nvidiaParallel, nvidiaProvider = getGpuOptions nvidiaPlatformName 16
//    let intelOptions, intelProvider = getGpuOptions intelPlatformName 16
//    let amdOptions, amdProvider = getGpuOptions amdPlatformName 16
//    let defaultOptions, dafeultProvider = getGpuOptions defaultPlatformName 16
    
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
    
//                
//    printfn "AMD:"
//    printParams amdProvider
//    printfn "Intel:"
//    printParams intelProvider
    printfn "NVIDIA:"
    printParams nvidiaProvider
    
    0

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
    
    let printMatrix (matrix: ProbabilityMatrix.T) strLen searchLen = 
        let rowLength = matrix.GetLength(0)
        let colLength = matrix.GetLength(1)
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
        let rowLength = matrix.GetLength(0)
        let colLength = matrix.GetLength(1)
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
    
    let check str searchLen = 
        let toCheck    = recognize (Options.empty Algorithm.Okhotin) str rules nonterminals S searchLen 
        let toCheckBFS = recognize nvidiaParallel str rules nonterminals S searchLen 
        assert (isAnswerValid toCheck (String.length str) searchLen)
        assert (isAnswerValid toCheckBFS (String.length str) searchLen)
        let sameCells cell = 
            //                (Probability.unwrap toCheck.[i, j]) = (Probability.unwrap toCheckBFS.[i, j])
            (Probability.unwrap toCheck.[cell]) - (Probability.unwrap toCheckBFS.[cell]) < 0.0000000001
        let sameAnswers = 
            [ 0..toCheck.GetLength(0) - 1 ]
            |> Seq.forall (fun i -> (Seq.forall (fun j -> sameCells <| Cell.create i j) [ 0..toCheck.GetLength(0) - 1 ])) 
                
        assert sameAnswers
//        printMatrix toCheck (String.length str) searchLen 
//        printMatrix toCheckBFS (String.length str) searchLen 

    let checkOneType task check taskType str searchLen = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        //            List.iter (fun _ -> task str searchLen |> ignore) [1..10] 
        let toCheck = task str searchLen
        stopWatch.Stop()
        printfn "type: %s, str length: %i, search length: %i, time(ms): %f." taskType (String.length str) searchLen 
            stopWatch.Elapsed.TotalMilliseconds
    
    let checkTime str searchLen = 
//        checkOneType 
//            (fun str searchLen -> recognize { nvidiaOptions with Parallel = Some { MinMatrixSize = 1 } } str rules nonterminals S searchLen ) 
//            (fun toCheck -> isAnswerValid toCheck) "nvidia" str searchLen
        checkOneType 
            (fun str searchLen -> recognize { Options.empty Algorithm.Okhotin with Parallel = Some { MinMatrixSize = 8 }; Fast = Some { MinMatrixSize = 64 } } str rules nonterminals S searchLen ) 
            (fun toCheck -> isAnswerValid toCheck) "okhotin" str searchLen
        checkOneType 
            (fun str searchLen -> recognize { Options.empty Algorithm.Modified with Parallel = Some { MinMatrixSize = 1 }; Fast = Some { MinMatrixSize = 64 } } str rules nonterminals S searchLen ) 
            (fun toCheck -> isAnswerValid toCheck) "my" str searchLen
//        checkOneType 
//            (fun str searchLen -> CYKMatrixBFS.recognize str rules nonterminals S searchLen { Options.empty with Fast = Some { MinMatrixSize = 64 }; Parallel = Some { MinMatrixSize = 1 } }) 
//            (fun toCheck -> isAnswerValid toCheck) "fast, parallel" str searchLen
    

//    check "abb"      2
//    check "abb"      3    
//    check "aaabbcc"  5
//    check "aaabb"    5
//    check "aaaaabbb" 6
//    check "aaaabbbbbb" 6
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

//    check (String.replicate 40 "abb") 100
    checkTime (String.replicate 300 "abb") 800
    //        checkTime ((String.replicate 511 "abbb") + "abb") 50

    //        check "aabb"
    //        check "abb"    
    //        check "aaabbcc"
    //        check "baaabbcc"
    //        check "aaaabbcc"

    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    System.Console.ReadLine() |> ignore
    0
