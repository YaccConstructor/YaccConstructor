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
    use out = new System.IO.StreamWriter (srcFileName + ".translate.fs")
    let tab = 4
    let print (x : 'a) =
        fprintf out x
    let printInd num (x : 'a) =
        print "%s" (String.replicate (tab * num) " ")
        print x
    let rules = grammar.rules
    let indexator = grammar.indexator
    let count = Array.zeroCreate indexator.nonTermCount
    let index = Array.zeroCreate rules.rulesCount
    for i = 0 to rules.rulesCount-1 do
        let nonTerm = rules.leftSide i
        index.[i] <- count.[nonTerm]
        count.[nonTerm] <- count.[nonTerm] + 1
    let declareNonTermsArrays =
        [for i = 0 to indexator.nonTermCount - 1 do
            yield wordL <| sprintf "let _rnglr_translate_%s = Array.zeroCreate %d" (indexator.indexToNonTerm i) count.[i]]
        |> aboveListL
    
    let getProductionLayout num = function
        | PRef (name, args) ->
            incr num

        | PSeq (s, ac) -> ()
    let getRuleLayout (rule : Rule.t<Source.t,Source.t>) =
        let header =
            rule.args
            |> List.map (fun arg -> sprintf "fun %s ->" (Source.toString arg))
            |> String.concat " "
            |> (fun x -> wordL <| (x + " " + sprintf "fun (_rnglr_children : 'a[]) ->"))
        header
        //@@-- getProductionLayout (ref -1) rule.body
    let res =
        let rules = 
            srcGrammar
            |> List.mapi
                (fun i rule ->
                    (wordL <| sprintf "_rnglr_translate_%s.[%d] <- (" (if i = 0 then "let" else "and") i)
                    @@-- (getRuleLayout rule)
                    @@ wordL ")")
            |> aboveListL
(*        let translates =
            grammar.indexator.nonTermCount
            |> (fun s -> s @@ tail)
            |> Display.layout_to_string FormatOptions.Default
        translates
        *)
        rules
    out.Write res
    out.Close()
    ()