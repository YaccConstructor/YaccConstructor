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

let justParse (path:string) =
    let t = System.DateTime.Now
    use reader = new System.IO.StreamReader(path)
    let lexbuf = LexBuffer<_>.FromTextReader reader

    let i = ref 0L
    let traceStep = 10000L
    let timeOfIteration = ref System.DateTime.Now        


    let allTokens = 
        seq{
            while not lexbuf.IsPastEndOfStream do 
                i := 1L + !i
                if (!i % traceStep) = 0L then 
                  let oldTime = !timeOfIteration
                  timeOfIteration := System.DateTime.Now
                  let seconds = (!timeOfIteration - oldTime).TotalSeconds
                  printfn "tkn# %10d Tkns/s:%8.0f - p" i.Value (float traceStep / seconds)

                yield Lexer.tokens lexbuf}

    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }

    let res = buildAst allTokens
    printfn "time = %A" (System.DateTime.Now - t)
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