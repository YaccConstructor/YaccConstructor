//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Generators.RNGLR.AbstractParser

open QuickGraph
open QuickGraph.Algorithms
open AbstractParsing.Common

type Parser<'token>() =

    let parse buildAst (inGraph:ParserInputGraph<'token>): Parser.ParseResult<'token> =
        let ts =  inGraph.TopologicalSort() |> Array.ofSeq
        let ids = dict (ts |> Array.mapi (fun i v -> v,i))
        let tokens = 
            ts
            |> Seq.mapi (fun i v -> i,(inGraph.OutEdges v |> (Seq.map (fun e -> e.Tag,(ids.[e.Target]))) |> Array.ofSeq))
            //|> Seq.filter (fun (_,a) -> a.Length > 0)
            //|> Seq.concat
                
        buildAst tokens
    
    member this.Parse = parse