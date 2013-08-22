//  RNGLRAbstractParser.fs contains implementation of interpreter for abstract parser based on RNGLR.
//
//  Copyright 2013 Semyon Grigorev <rsdpisuy@gmail.com>
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

module Yard.Generators.RNGLR.AbstractParser

open QuickGraph
open QuickGraph.Algorithms
open AbstractParsing.Common

type Parser<'token>() =
//
//    let parse buildAst (inGraph:Option<ParserInputGraph<'token>>): Parser.ParseResult<'token> =
//        let ts =  inGraph.Value.TopologicalSort() |> Array.ofSeq
//        let ids = dict (ts |> Array.mapi (fun i v -> v,i))
//        let tokens = 
//            ts
//            |> Seq.mapi (fun i v -> i,(inGraph.Value.OutEdges v |> (Seq.map (fun e -> e.Tag,(ids.[e.Target]))) |> Array.ofSeq))
//            //|> Seq.filter (fun (_,a) -> a.Length > 0)
//            //|> Seq.concat
//                
//        buildAst tokens

    member this.xx () = 1
    member this.Parse_x((inGraph(*:ParserInputGraph<'token>*))) = None//parse buildAst None