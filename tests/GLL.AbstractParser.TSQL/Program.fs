module GLLAbstractParserTSQL

open System.IO
open NUnit.Framework
open Microsoft.FSharp.Text.Lexing
open AbstractAnalysis.Common
open Yard.Generators.GLL.AbstractParser

open GLL.MSParser
open YC.TSQLLexer

let needChangeDirectory = Directory.GetCurrentDirectory().Contains("ReSharper")
let dataDir =
    if needChangeDirectory
    then @"E:/YCCW/Current_build/YaccConstructor/tests/data/GLL/SQL_tests/"
    else @"./data/GLL/SQL_tests/"

let inputFromFile path =
    let res =
        let content = System.IO.File.ReadAllText(path)
        let reader = new System.IO.StringReader(content)
        LexBuffer<_>.FromTextReader(reader)
    try
        let tokenSeq = seq {
                while not res.IsPastEndOfStream do
                    yield tokens res
            }
        tokenSeq
        |> Seq.filter Option.isSome
        |> Seq.map (fun (Some x) ->
            tokenToNumber x)
        |> Seq.toArray
    with 
        | Failure msg -> 
            Assert.Fail(sprintf "Tokenization failed: %s" msg)
            reraise()

let parseTime path isTrue buildTree =
    let parser = parserSource
    let tokens = inputFromFile (dataDir + path)
    let tl = Array.length tokens
    let input = new LinearInput(tokens)
    
    let watch = System.Diagnostics.Stopwatch.StartNew()
    let result = 
        if buildTree
        then 
            try 
                buildAst parser input |> ignore
                true
            with
                | _ ->  false
        else isParsed parser input
    watch.Stop()

    Assert.AreEqual(result, isTrue, "Parser failed")
    watch.Elapsed.TotalMilliseconds

let isParsed path isTrue =
    parseTime path isTrue false
    |> ignore

[<TestFixture>]
type ``GLL TSQL tests``() =
    [<Test>]
    member test.``01_Simple_select``() =
        isParsed "01_Simple_select.sql" true
        
    [<Test>]
    member test.``02_Recreate_table``() =
        isParsed "02_Recreate_table.sql" true
        
    [<Test>]
    member test.``03_Basics_insert``() =
        isParsed "03_Basics_insert.sql" true
        
    [<Test>]
    member test.``04_Simple_function``() =
        isParsed "04_Simple_function.sql" true
        
    [<Test>]
    member test.``05_While_if_case``() =
        isParsed "05_While_if_case.sql" true
        
    [<Test>]
    member test.``06_With``() =
        isParsed "06_With.sql" true
        
    [<Test>]
    member test.``07_Cursor``() =
        isParsed "07_Cursor.sql" true
        
    [<Test>]
    member test.``08_Incorrect_select``() =
        isParsed "08_Incorrect_select.sql" false
        
    [<Test>]
    member test.``09_Incorrect_if``() =
        isParsed "09_Incorrect_if.sql" false
        
    [<Test>]
    member test.``10_Incorrect_function``() =
        isParsed "10_Incorrect_function.sql" false
        
    [<Test>]
    member test.``11_Declare_table``() =
        isParsed "11_Declare_table.sql" true

    [<Test>]
    [<Ignore("Takes too much time")>]
    member test.``Performance_test_1``() =
        let path = "Big_data.sql"
        printfn "\"%s\" isParsed time = %f" path
        <|  parseTime path true false

    [<Test>]
    [<Ignore("Takes too much time")>]
    member test.``Performance_test_2``() =
        let path = "Big_data_x5.sql"
        printfn "\"%s\" isParsed time = %f" path
        <|  parseTime path true false

    [<Test>]
    [<Ignore("Takes too much time")>]
    member test.``Performance_test_3``() =
        let path = "Big_data_x2.sql"
        printfn "\"%s\" buildAst time = %f" path
        <|  parseTime path true true
