//  AST.fs contains description of AST built with GNESCC
//
//  Copyright 2009,2010,2011 Semen Grigorev <rsdpisuy@gmail.com>
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

module Yard.Generators.GNESCCGenerator.AST

open Yard.Core.CompareHelper

type AST<'lexeme, 'nodeVal, 'trace 
        when 'lexeme : equality 
        and  'lexeme : comparison
        and  'trace  : equality> =
         
        | Node  of   List<AST<'lexeme, 'nodeVal, 'trace> (*ref*)>
                   * int
                   *'trace                   
        | Leaf  of int * 'lexeme
             
let rec private dumpTree i item =
    let iter i = String.replicate i "    "
    match item with
    | Node (lst, tag, trace) -> 
        let trace = trace.ToString()
        [iter i;"<NODE name=\""; string tag; "\" trace=\""; trace ; "\">\n"]
        @(List.map (fun x -> dumpTree (i+1) x) lst)
        @[iter i;"</NODE>\n"]
        |> String.concat "" 

    | Leaf (tag, value)     ->
        String.concat "" [iter i;"<LEAF name=\""; string tag; "\"/>\n"]        

let PrintTree tree = System.Console.WriteLine (dumpTree 0 tree)