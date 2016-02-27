open System
open System.IO
open System.Diagnostics
open System.Collections.Generic
open YC.FST.AbstractLexing.Interpreter
open YC.FST.AbstractLexing.Tests.CommonTestChecker
open YC.FST.AbstractLexing.Tests.Calc
open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa
open QuickGraph.FSA.FsaApproximation
open QuickGraph.FST.Tests.GraphBasedFstTestData

let benchmark func iterations =
    GC.Collect() // give the test as good a chance as possible of avoiding garbage collection
    func() |> ignore // run once outside of loop to avoid initialization costs
    let sw = Stopwatch.StartNew()
    for i = 0 to iterations do
        func() |> ignore
    sw.Stop()
    double(sw.ElapsedMilliseconds) / double(iterations)

let getFST basePath path =
    let graphAppr = loadDotToQG basePath path
    let graphFsa = graphAppr.ApprToFSA()
    FST<_,_>.FSAtoFST(graphFsa, transform, smblEOF)

let getCalcFST = "../../../../Tests/AbstractLexing/DOT" |> getFST
let getTSQLFST = "../../../TSQL.Test/DotTSQL" |> getFST

let calcLexer = YC.FST.AbstractLexing.CalcLexer.fstLexer()
let calcAlphabet = YC.FST.AbstractLexing.CalcLexer.alphabet()
let TSQLLexer = YC.TSQLLexer.fstLexer()
let TSQLAlphabet = YC.TSQLLexer.alphabet()

let compose graphFST lexer alphabet = FST<_,_>.Compos(graphFST, lexer, alphabet) |> ignore
let optimalCompose graphFST lexer alphabet = FST<_,_>.optimalCompose(graphFST, lexer, alphabet) |> ignore

let calcCompose graphFST = compose graphFST calcLexer calcAlphabet 
let calcOptimalCompose graphFST = optimalCompose graphFST calcLexer calcAlphabet
let TSQLCompose graphFST = compose graphFST TSQLLexer TSQLAlphabet
let TSQLOptimalCompose graphFST = optimalCompose graphFST TSQLLexer TSQLAlphabet

let calcTests = ["test_0.dot"; "test_1.dot"; "test_2.dot"; "test_3.dot"]
let TSQLTests = ["test_tsql_1.dot"; "test_tsql_2.dot"]
let manuallyCreatedTests = [fstCompos1, fstCompos2; fstCompos12, fstCompos22; fstCompos13, fstCompos22]

[<EntryPoint>]
let main argv = 
    let runLangTests lang tests getFST compose optimalCompose =
        for test in tests do
            try
                let fst = getFST test
                printfn "Processing %A" test
                printfn "Average time for compose: %A" (benchmark (fun () -> compose fst) 10)
                printfn "Average time for optimal compose: %A" (benchmark (fun () -> optimalCompose fst) 10)
            with
                | e -> printfn"%s is not %s compliant! Error: %A" test lang e.Message
    let runManuallyCreatedTests (tests : list<FST<_,_>*FST<_,_>>) = 
        for (fst1, fst2) in tests do
            let alphabet = new HashSet<_>()
            for edge in fst2.Edges do
                alphabet.Add(fst edge.Tag) |> ignore
            printfn "Processing manually created FSTs"
            printfn "Average time for compose: %A" (benchmark (fun () -> compose fst1 fst2 alphabet) 100)
            printfn "Average time for optimal compose: %A" (benchmark (fun () -> optimalCompose fst1 fst2 alphabet) 100)
    if Array.exists (fun arg -> arg.Equals "-d") argv then
        runLangTests "Calc" calcTests getCalcFST calcCompose calcOptimalCompose
        runLangTests "TSQL" TSQLTests getTSQLFST TSQLCompose TSQLOptimalCompose
        runManuallyCreatedTests manuallyCreatedTests
    if Array.exists (fun arg -> arg.Equals "-f") argv then
        try 
            let path = argv.[Array.findIndex (fun x -> x.Equals("-f")) argv + 1]
            let folder = new DirectoryInfo(path)
            let getTSQLFST = path |> getFST
            let externalTSQLTests = [for x in folder.GetFiles() do if x.Extension.Equals(".dot") then yield x.Name]
            runLangTests "TSQL" externalTSQLTests getTSQLFST TSQLCompose TSQLOptimalCompose
        with
            | _ -> printfn "Wrong folder!"
    0
