//  Parser.fs contains type, describing information, written to file as result of generation
//     and used by Parser and Translator.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
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

module Yard.Generators.RNGLR.TranslatorPrinter

open Yard.Generators.RNGLR.FinalGrammar
open System.Collections.Generic
open Yard.Generators.RNGLR
open Yard.Core.IL
open Yard.Core.IL.Production
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let printTranslator (grammar : FinalGrammar) (srcGrammar : Rule.t<Source.t,Source.t> list)
        tail (srcFileName : string) (tokenTypeOpt : string option) =
(*    let getProductionLayout = function
        | PRef (name, args) -> 
        | PSeq (s, ac) -> ()
    let getRuleLayout (rule : Rule.t<Source.t,Source.t>) =
        let header =
            rule.args
            |> List.map (fun arg -> wordL <| sprintf "fun %s ->" (Source.toString arg)) |> spaceListL
        header
        @@-- getProductionLayout rule.body
    let res =
        let rules = 
            srcGrammar
            |> List.mapi
                (fun i rule ->
                    (wordL <| sprintf "%s _rnglr_rule_%d = " (if i = 0 then "let" else "and") i)
                    @@-- (getRuleLayout rule))
            |> aboveListL
        let translates =
            grammar.indexator.nonTermCount
        |> (fun s -> s @@ tail)
        |> Display.layout_to_string FormatOptions.Default
*)
    use out = new System.IO.StreamWriter (srcFileName + ".translate.fs")
//    out.Write res
    out.Close()
    ()