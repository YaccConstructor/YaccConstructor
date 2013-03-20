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
    use reader = new System.IO.StreamReader(path)
    let lexbuf = LexBuffer<_>.FromTextReader reader
    let allTokens = seq{while not lexbuf.IsPastEndOfStream do yield Lexer.tokens lexbuf}

    let translateArgs = {
        tokenToRange = fun x -> 0,0
        zeroPosition = 0
        clearAST = false
        filterEpsilons = true
    }

    buildAst allTokens

let Parse (srcFilePath:string) =    
    match justParse srcFilePath with
    | Yard.Generators.RNGLR.Parser.Error (num, tok, msg,dbg) ->
        printfn "Error in file %s on position %s on Token %A: %s" srcFilePath (tokenPos tok) (tok.GetType()) msg
        //dbg.lastTokens(10) |> printfn "%A"
        dbg.drawGSSDot @"..\..\stack.dot"
    | Yard.Generators.RNGLR.Parser.Success ast ->
        ast.collectWarnings (fun x -> 0,0)
        |> ResizeArray.iter (fun (pos, prods) -> ())
        defaultAstToDot ast @"..\..\ast.dot"
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