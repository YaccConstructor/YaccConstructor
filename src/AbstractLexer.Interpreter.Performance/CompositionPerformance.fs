open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Collections 
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.Calc
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open QuickGraph
open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FST.Tests.GraphBasedFstTestData

let benchmark func iterations =
    GC.Collect() // give the test as good a chance as possible of avoiding garbage collection
    func() |> ignore // run once outside of loop to avoid initialization costs
    let sw = Stopwatch.StartNew()
    for i = 0 to iterations do
        func() |> ignore
    sw.Stop()
    double(sw.ElapsedMilliseconds) / double(iterations)

let getFST path =
    let graph = loadDotToQG path
    let fst = FST<_,_>.FSAtoFST(approximateQG(graph), transform, smblEOF)
    printfn "%A vertices, %A edges in approximation" graph.VertexCount graph.EdgeCount
    printfn "%A vertices, %A edges in FST" fst.VertexCount fst.EdgeCount
    fst

let calcLexer = YC.FST.AbstractLexing.CalcLexer.fstLexer()
let calcAlphabet = YC.FST.AbstractLexing.CalcLexer.alphabet()
let TSQLLexer = YC.TSQLLexer.fstLexer()
let TSQLAlphabet = YC.TSQLLexer.alphabet()

let oldCompose graphFST lexer alphabet = FST<_,_>.oldCompose(graphFST, lexer, alphabet) |> ignore
let compose graphFST lexer alphabet = FST<_,_>.Compose(graphFST, lexer, alphabet) |> ignore

let calcOldCompose graphFST = oldCompose graphFST calcLexer calcAlphabet 
let calcCompose graphFST = compose graphFST calcLexer calcAlphabet
let TSQLOldCompose graphFST = oldCompose graphFST TSQLLexer TSQLAlphabet
let TSQLCompose graphFST = compose graphFST TSQLLexer TSQLAlphabet

let getTests path fileList =
    [for x in fileList do yield path + "/" + x]

let calcTests = getTests "../../../../Tests/AbstractLexing/DOT" ["test_0.dot"; "test_1.dot"; "test_2.dot"; "test_3.dot"]
let TSQLTests = getTests "../../../TSQL.Test/DotTSQL" ["test_tsql_1.dot"; "test_tsql_2.dot"; "test_tsql_3.dot"; "test_tsql_4.dot"; "test_tsql_5.dot"; "test_tsql_6.dot"; "test_tsql_7.dot"]
let manuallyCreatedTests = [fstCompos1, fstCompos2; fstCompos12, fstCompos22; fstCompos13, fstCompos22]

[<EntryPoint>]
let main argv = 
    let runLangTests lang tests oldCompose compose =
        for test in tests do
            printfn "Processing %A:" test
            try
                let fst = getFST test
                try
                    let oldComposeTime = benchmark (fun () -> oldCompose fst) 2
                    let composeTime = benchmark (fun () -> compose fst) 2
                    printfn "Average time for compose: %A" oldComposeTime
                    printfn "Average time for optimal compose: %A" composeTime
                    printfn "Ratio: %A\n" (oldComposeTime / composeTime)
                with
                    | e -> printfn"%s is not %s compliant! Error: %A\n" test lang e.Message
            with
                | e -> printfn"%s is not a valid .dot file! Full error: %A\n" test e
    let runManuallyCreatedTests (tests : list<FST<_,_>*FST<_,_>>) = 
        for (fst1, fst2) in tests do
            let alphabet = new HashSet<_>()
            for edge in fst2.Edges do
                alphabet.Add(fst edge.Tag) |> ignore
            printfn "Processing manually created FSTs:"
            let oldComposeTime = benchmark (fun () -> oldCompose fst1 fst2 alphabet) 100
            let composeTime = benchmark (fun () -> compose fst1 fst2 alphabet) 100
            printfn "Average time for compose: %A" oldComposeTime
            printfn "Average time for optimal compose: %A" composeTime
            printfn "Ratio: %A\n" (oldComposeTime / composeTime)
    if Array.exists (fun arg -> arg.Equals "-d") argv then
        runLangTests "Calc" calcTests calcOldCompose calcCompose
        runLangTests "TSQL" TSQLTests TSQLOldCompose TSQLCompose
        runManuallyCreatedTests manuallyCreatedTests
    if Array.exists (fun arg -> arg.Equals "-f") argv then
        try
            let path = argv.[Array.findIndex (fun x -> x.Equals("-f")) argv + 1]
            let folder = new DirectoryInfo(path)
            let externalTSQLTests = [for x in folder.GetFiles() do if x.Extension.Equals(".dot") then yield x.FullName]
            runLangTests "TSQL" externalTSQLTests TSQLOldCompose TSQLCompose
        with
            | _ -> printfn "Wrong folder!"
    0
