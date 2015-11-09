//  Driver.fs contains entry point of MS-SQL parser.
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module YC.ReSharper.Languages.TSQL.RnglrChunkDriver

open Microsoft.FSharp.Text.Lexing
open Yard.Examples.MSParser
open LexerHelper
open System
open System.IO
open Yard.Generators.Common.AST
open LexerHelper
open Yard.Utils.SourceText
open Yard.Utils.StructClass
open Yard.Utils.InfoClass

(*let tokenize lexerInputGraph =
    let eof = Yard.Examples.MSParserAbstract.RNGLR_EOF(Yard.Utils.SourceText.SourceText(),[||])
    LexerAbstract._fslex_tables.Tokenize(LexerAbstract.fslex_actions_tokens, lexerInputGraph, eof)*)

(*let parserAbstr = new Yard.Generators.RNGLR.AbstractParser.Parser<_>()*)


let getTokenName = tokenToNumber >> numToString

//Not abstract parsing
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

    let start = System.DateTime.Now
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
    
    let res = 
       Yard.Examples.MSParser.buildAst allTokens
    printfn "Time for parse file %s = %A" file (System.DateTime.Now - start)
    res

let p = new ProjInfo()
let mutable counter = 1<id>

let printError (tok:Yard.Examples.MSParser.Token) msg (srcFilePath:string)  =
    let coordinates = 
        let x,y = tokenPos tok
        let x = p.GetCoordinates x
        let y = p.GetCoordinates y
        sprintf "(%A,%A) - (%A,%A)" x.Line x.Column y.Line y.Column
    let data =
        let d = tokenData tok
        if isLiteral tok then ""
        else (d :?> SourceText).text
    let name = tok |> tokenToNumber |> numToString
    printfn "Error in file %s on position %s on Token %s %s: %s" srcFilePath coordinates name data msg

let Parse (srcFilePath:string) = 
    let StreamElement = new StreamReader(srcFilePath, System.Text.Encoding.UTF8)  
    let map = p.GetMap StreamElement
    Lexer.id <- counter
    p.AddLine counter map
    counter <- counter + 1<id>
   
    match justParse srcFilePath with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg, dbg, _) ->
        for toks in tok do 
            printError toks msg srcFilePath
        dbg.drawGSSDot @"..\..\stack.dot"   
    | Yard.Generators.RNGLR.Parser.Success (ast, _, _) ->
        let GC_Collect () = 
            GC.Collect()    
            GC.WaitForPendingFinalizers()
            GC.GetTotalMemory(true)
        GC_Collect() |> printfn "%A" 

        let errors = ast.collectErrors (tokenPos)
        for x, y, toks in errors do
            let x = p.GetCoordinates x
            let y = p.GetCoordinates y
            let tokToString token = token |> tokenToNumber |> numToString
            printf "Error on position (%A, %A) - (%A, %A) on token(s) : " x.Line x.Column y.Line y.Column
            toks |> Array.map (fun x -> unbox x |> tokToString) |> Array.iter (printfn " %s ")
            printfn ""

        ast.collectWarnings (tokenPos >> fun (x,y) -> let x = RePack x in x.Line + 1<line> |> int, int x.Column)
        |> Seq.groupBy snd
        |> Seq.sortBy (fun (_,gv) -> - (Seq.length gv))
        |> Seq.iter (fun (prods, gv) -> 
            printfn "conf# %i  prods: %A" (Seq.length gv) prods
            gv |> (fun s -> if Seq.length s > 5 then Seq.take 5 s else s) |> Seq.map fst |> Seq.iter (printfn "    %A")) 

(*let parseAbstract (*parser:Yard.Generators.RNGLR.AbstractParser.Parser<_>*) =
    
    fun parserInputGraph -> parserAbstr.Parse buildAstAbstract parserInputGraph*)