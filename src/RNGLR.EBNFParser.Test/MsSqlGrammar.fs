module RNGLREBNFParserTest

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.EBNF.Parser
open Yard.Generators.RNGLR.EBNF
open NUnit.Framework
open YC.Tests.Helper
open Yard.Examples.MSParser
open Lexer
open Yard.Utils.SourceText

let getTokenName = tokenToNumber >> numToString

let justParse (file:string) =
    let lastTokenNum = ref 0L
    let traceStep = 50000L
    let c = ref 0
    use reader = new System.IO.StreamReader(file)

    let tokenizerFun = 
        let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromTextReader reader
        lexbuf.EndPos <- { pos_bol = 0; pos_fname=""; pos_cnum=0; pos_lnum=1 }    
        let prevToken = ref None            
        let timeOfIteration = ref System.DateTime.Now
        fun (chan:MailboxProcessor<array<_>>) ->
        let post = chan.Post
        async {
            try                    
                while not lexbuf.IsPastEndOfStream do
                    let count = ref 0L
                    let buf = int traceStep |> Array.zeroCreate
                    while !count < traceStep && not lexbuf.IsPastEndOfStream do
                        lastTokenNum := 1L + !lastTokenNum                        
                        buf.[int !count] <- Lexer.tokens lexbuf
                        count := !count + 1L                    
                    let oldTime = !timeOfIteration
                    timeOfIteration := System.DateTime.Now
                    let mSeconds = int64 ((!timeOfIteration - oldTime).Duration().TotalMilliseconds)
                    printfn "tkn# %10d Tkns/s:%8d - l" lastTokenNum.Value (1000L * traceStep/ (mSeconds + 1L))
                    if int64 chan.CurrentQueueLength > 3L then                        
                        int (int64 chan.CurrentQueueLength * mSeconds)  |> System.Threading.Thread.Sleep
                    post buf
                            
            with e -> printfn "LexerError:%A" e.Message
        }   

    //let start = System.DateTime.Now
    use tokenizer =  MailboxProcessor<_>.Start(tokenizerFun)
    let lastTokenNum = ref 0L    
    let timeOfIteration = ref System.DateTime.Now

    let allTokens = 
        seq{
            while true do
                let arr = tokenizer.Receive 100000 |> Async.RunSynchronously
                lastTokenNum := !lastTokenNum + int64 arr.Length
                if (!lastTokenNum % (traceStep)) = 0L then                 
                    let oldTime = !timeOfIteration
                    timeOfIteration := System.DateTime.Now
                    let mSeconds = int64 ((!timeOfIteration - oldTime).Duration().TotalMilliseconds)
                    printfn "tkn# %10d Tkns/s:%8d - p" lastTokenNum.Value (1000L * traceStep / (mSeconds + 1L))
                yield! arr
                }

    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }

    let start = System.DateTime.Now
    let res = 
       Yard.Examples.MSParser.buildAst allTokens
    printfn "Time for parse file %s = %A" file (System.DateTime.Now - start)
    res

[<TestFixture>]
type ``RNGLREBNF parser for MsSql grammar`` () =
    [<Test>]
    member test.``Test`` () =
        Assert.AreEqual(2, 2)