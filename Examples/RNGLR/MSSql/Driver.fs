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
open Yard.Utils.SourceText
open Yard.Utils.StructClass
open Yard.Utils.InfoClass
open System
open System.IO



let justParse (path:string) =
    let lastTokenNum = ref 0L
    let traceStep = 50000L
    let c = ref 0

    use reader = new System.IO.StreamReader(path)

    let tokenizerFun = 
        let lexbuf = Lexing.LexBuffer<_>.FromTextReader reader
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
//                            let r = !c,[|Lexer.tokens lexbuf, !c+1|]
//                            incr c
//                            r
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
    //let lexbuf = Lexing.LexBuffer<_>.FromTextReader reader
    let allTokens = 
        seq{
            while true do
                let arr = tokenizer.Receive 100000 |> Async.RunSynchronously
                lastTokenNum := !lastTokenNum + int64 arr.Length
                if (!lastTokenNum % (traceStep)) = 0L then                 
                    let oldTime = !timeOfIteration
                    timeOfIteration := System.DateTime.Now
                    let mSeconds = int64 ((!timeOfIteration - oldTime).Duration().TotalMilliseconds)
                    printfn "tkn# %10d Tkns/s:%8d - p" lastTokenNum.Value (1000L * traceStep/ (mSeconds + 1L))
                yield! arr
                //yield Lexer.tokens lexbuf
                }

    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }

    let res = 
        // Parse error on Token RNGLR_EOF
//        buildAstAbstract (allTokens |> Seq.takeWhile (fun t -> box t <> null) 
//                                    |> Seq.map (fun t -> let r = !c,[|t,!c+1|] in incr c;r))
        // Input was fully processed but it's not complete correct string
//        buildAstAbstract (allTokens |> Seq.takeWhile (fun t -> match t with
//                                                               | Token.RNGLR_EOF _ -> false
//                                                               | _ -> true) 
//                                    |> Seq.map (fun t -> let r = !c,[|t,!c+1|] in incr c;r))
        buildAst allTokens
    printfn "Time for parse file %s = %A" path (System.DateTime.Now - start)
    res

let p = new ProjInfo()
let mutable counter = 1<id>

let Parse (srcFilePath:string) = 
    let StreamElement = new StreamReader(srcFilePath, System.Text.Encoding.UTF8)  
    let map = p.GetMap StreamElement
    //Array.iter (printfn "%A") map
    Lexer.id <- counter
    p.AddLine counter map
    counter <- counter + 1<id>
    //Lexer.id <- from (ProjInfo)
    match justParse srcFilePath with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg, dbg, _) ->
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
        //dbg.lastTokens(10) |> printfn "%A"
        dbg.drawGSSDot @"..\..\stack.dot"
    | Yard.Generators.RNGLR.Parser.Success (ast, _) ->
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
        //defaultAstToDot ast @"..\..\ast.dot"
        //ast.ChooseLongestMatch()
        //let translated = translate translateArgs ast : list<Script>            
        //printfn "%A" translated
        //translated.Head  
        
let ParseAllDirectory (directoryName:string) =
    System.IO.Directory.GetFiles directoryName
    |> Array.iter Parse

do 
    let inPath = ref @"test.sql" 
    //let inPath = ref @"..\..\..\..\..\Tests\Materials\ms-sql\sysprocs\sp_addserver.sql"
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