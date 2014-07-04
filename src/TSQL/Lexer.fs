# 1 "Lexer.fsl"
 
//  UserLexer.fs contains simple lexer for testing.
//
//  for build:  fslex Lexer.fsl --unicode -o Lexer.fs
//
//  Copyright 2011, 2012 Semen Grigorev <rsdpisuy@gmail.com>
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

module Lexer
//open Microsoft.FSharp.Text.Lexing
//open Microsoft.FSharp.Text
open Microsoft.FSharp.Reflection
open Yard.Examples.MSParser
open LexerHelper
open System
open Yard.Utils.StructClass
open AbstractAnalysis.Common

let mutable id = 0<id>


# 37 "Lexer.fs"
let trans : uint16[] array = 
    [| 
    (* State 0 *)
     [| 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 2us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 2us; 7us; 7us; 5us; 7us; 5us; 7us; 7us; 3us; 3us; 3us; 3us; 3us; 4us; 3us; 7us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 7us; 3us; 3us; 3us; 3us; 7us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 3us; 7us; 3us; 7us; 5us; 7us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 7us; 7us; 7us; 3us; 7us; 65533us; 5us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 6us; |];
    (* State 1 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 2 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 11us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 11us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 3 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 4 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 9us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 5 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 8us; 65535us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 6 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 7 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 8 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 8us; 65535us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 8us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 9 *)
     [| 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 65535us; 10us; 10us; 65535us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 65533us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 65535us; |];
    (* State 10 *)
     [| 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 65535us; 10us; 10us; 65535us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 65533us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 65535us; |];
    (* State 11 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 11us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 11us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    (* State 12 *)
     [| 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65533us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |];
    |] 
let actions : uint16[] = [|65535us; 0us; 1us; 2us; 2us; 2us; 4us; 5us; 2us; 3us; 3us; 1us; 0us; |]
let _fslex_tables = AbstractLexer.Core.UnicodeTables.Create(trans,actions)
let rec _fslex_dummy () = _fslex_dummy() 
(* Rule tokens *)
let fslex_actions_tokens  _fslex_state lexeme brs =
  match _fslex_state with
  | 0 -> ( 
# 56 "Lexer.fsl"
                                 DEC_NUMBER((defaultSourceText id brs lexeme),brs )|> Some 
# 76 "Lexer.fs"
          )
  | 1 -> ( 
# 57 "Lexer.fsl"
                                 None 
# 81 "Lexer.fs"
          )
  | 2 -> ( 
# 73 "Lexer.fsl"
                           let tok = makeIdent false lexeme (brs, brs)
                           Some tok 
# 87 "Lexer.fs"
          )
  | 3 -> ( 
# 75 "Lexer.fsl"
                                        None (* parser knows nothing about comments *) 
# 92 "Lexer.fs"
          )
  | 4 -> ( 
# 77 "Lexer.fsl"
                        RNGLR_EOF((defaultSourceText id brs  ""),brs) |> Some 
# 97 "Lexer.fs"
          )
  | 5 -> ( 
# 78 "Lexer.fsl"
                       raise (LexerError(lexeme,box brs)) 
# 102 "Lexer.fs"
          )
  | _ -> failwith "tokens"

# 3000000 "Lexer.fs"
