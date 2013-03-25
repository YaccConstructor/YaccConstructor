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

module MSSqlParser

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.RNGLR.AST
open Yard.Examples.MSParser
open LexerHelper

let lastTokenNum = ref 0L
let traceStep = 1000L

let justParse (path:string) =
    use reader = new System.IO.StreamReader(path)

    let tokenizerFun = 
        let lexbuf = Lexing.LexBuffer<_>.FromTextReader reader
        lexbuf.EndPos <- { pos_bol = 0; pos_fname=""; pos_cnum=0; pos_lnum=1 }    
        let prevToken = ref None            
        let timeOfIteration = ref System.DateTime.Now
        fun (chan:MailboxProcessor<array<Token>>) ->
        let post = chan.Post
        async {
            try                    
                while not lexbuf.IsPastEndOfStream do
                    let count = ref 0L
                    let buf = int traceStep |> Array.zeroCreate
                    while !count < traceStep && not lexbuf.IsPastEndOfStream do
                        lastTokenNum := 1L + !lastTokenNum
                        if (!lastTokenNum % (traceStep)) = 0L then 
                            let oldTime = !timeOfIteration
                            timeOfIteration := System.DateTime.Now
                            let mSeconds = int64 ((!timeOfIteration - oldTime).Duration().TotalMilliseconds)
                            printfn "tkn# %10d Tkns/s:%8d - l" lastTokenNum.Value (1000L * traceStep/ mSeconds)                    
                            if int64 chan.CurrentQueueLength > 3L then                                                                                  
                                int (int64 chan.CurrentQueueLength * mSeconds / traceStep)  |> System.Threading.Thread.Sleep          
                        buf.[int !count] <- Lexer.tokens lexbuf
                        count := !count + 1L
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
                    printfn "tkn# %10d Tkns/s:%8d - p" lastTokenNum.Value (1000L * traceStep/ mSeconds)
                yield! arr}

    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }

    let res = buildAst allTokens
    printfn "Time for parse file %s = %A" path (System.DateTime.Now - start)
    res

let Parse (srcFilePath:string) =    
    match justParse srcFilePath with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,dbg) ->
        let print = tokenPos >> (fun(x,y) -> sprintf "(%i,%i) - (%i,%i)" (x.Line+1) x.Column (y.Line+1) y.Column)
        printfn "Error in file %s on position %s on Token %A: %s" srcFilePath (print tok) (tok.GetType()) msg
        //dbg.lastTokens(10) |> printfn "%A"
        dbg.drawGSSDot @"..\..\stack.dot"
    | Yard.Generators.RNGLR.Parser.Success ast ->
        ast.collectWarnings (tokenPos >> fun (x,y) -> x.Line, x.Column)
        |> Seq.groupBy snd
        |> Seq.sortBy (fun (_,gv) -> - (Seq.length gv))
        |> Seq.iter (fun (prods, gv) -> 
            printfn "conf# %i  prods: %A" (Seq.length gv) prods
            gv |> (fun s -> if Seq.length s > 5 then Seq.take 5 s else s) |> Seq.map fst |> Seq.iter (printfn "    %A"))
        //defaultAstToDot ast @"..\..\ast.dot"
        //ast.ChooseLongestMatch()
        //let translated = translate translateArgs ast : list<Script>            
        //printfn "%A" translated
        //translated.Head  
        
let ParseAllDirectory (directoryName:string) =
    System.IO.Directory.GetFiles directoryName
    |> Array.iter Parse

do 
    let inPath = ref @"..\..\..\..\..\Tests\Materials\ms-sql\sqlsrvanalysissrvcs\MonitoringSSAS\config_data_server\get_query_text.sql"
    let parseDir = ref false
    let commandLineSpecs =
        ["-f", ArgType.String (fun s -> inPath := s), "Input file."
         "-d", ArgType.String (fun s -> parseDir := true; inPath := s), "Input dir. Use for parse all files in specified directory."
         ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs

    !inPath
    |> if !parseDir
       then ParseAllDirectory
       else Parse
    
//@"D:\projects\YC\recursive-ascent\Tests\Materials\ms-sql\sysprocs\sp_addserver.sql" 
//@"..\..\..\..\..\Tests\Materials\ms-sql\sqlsrvanalysissrvcs\MonitoringSSAS\config_data_server\get_query_text.sql"
//@"C:\Users\Anastasiya\Desktop\Projects\Reengineering\recursive-ascent\Tests\materials\ms-sql\sysprocs\sp_addserver.sql"