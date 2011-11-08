//  GNESCCCoreTests.fs contains unit tests for GNESCCCore
//
//  Copyright 2010, 2011 Semen Grigorev <rsdpisuy@gmail.com>
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

module GNESCCCoreTests

open Microsoft.FSharp.Text.Lexing
open Yard.Generators.GNESCCGenerator
open NUnit.Framework
open Yard.Generators


let run path tables actions regexp lexerF =
    let buf =
        let content = System.IO.File.ReadAllText(path)    
        let reader = new System.IO.StringReader(content) 
        LexBuffer<_>.FromTextReader reader 

    let l = lexerF buf
    let parseRes = 
        let ti = new TableInterpreter(tables)
        ti.Run l
              
    Seq.map (fun tree -> ASTInterpretator.interp actions regexp tree) parseRes    
    |> Seq.filter (function | Success _ -> true | _ -> false)
    |> Seq.map (function | Success x -> x | _ -> failwith "Incorrect filter")
    |> List.ofSeq


[<TestFixture>]
type ``GNESCC core tests`` () =