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
open Yard.Generators.RNGLR.AST
open Yard.Core.IL
open Yard.Core.IL.Production
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let printTranslator (grammar : FinalGrammar) (srcGrammar : Rule.t<Source.t,Source.t> list) (out : System.IO.StreamWriter) =
    let tab = 4
    let print (x : 'a) =
        fprintf out x
    let printInd num (x : 'a) =
        print "%s" (String.replicate (tab * num) " ")
        print x

    let rules = grammar.rules
    let srcGrammar = Array.ofList srcGrammar
    let indexator = grammar.indexator
    let nonTermLim = indexator.nonTermCount - 1

    let resCycleName = "_rnglr_cycle_res"
    let nodeName = "_rnglr_node"
    let tokenCall = "_rnglr_translate_token"
    let ruleName = "_rnglr_rule_"
    let epsilonName = "_rnglr_epsilons"
    let childrenName = "_rnglr_children"
    let concatsName = "_rnglr_concats"
    //let pathToModule = "Yard.Generators.RNGLR.AST."

    let printArgsDeclare args= 
        args
        |> List.map (fun arg -> sprintf "fun %s ->" (Source.toString arg))
        |> String.concat " "
        |> fun s -> s.Trim()
        |> wordL

    let printArgsCallList args other = 
        args
        |> List.map Source.toString
        |> String.concat " "
        |> (fun x -> other + " " + x)

    let printArgsCallOpt args =
        match args with
        | None -> ""
        | Some str -> Source.toString str

    // Declare non-term arrays
    let count = Array.zeroCreate indexator.nonTermCount
    let index = Array.zeroCreate rules.rulesCount
    let args = Array.zeroCreate indexator.nonTermCount
    //args.[indexator.nonTermToIndex "error"] <- []
    for i = 0 to rules.rulesCount-1 do
        let nonTerm = rules.leftSide i
        index.[i] <- count.[nonTerm]
        count.[nonTerm] <- count.[nonTerm] + 1
        if index.[i] = 0 then
            args.[nonTerm] <- srcGrammar.[i].args

    let printArr (arr : 'a[]) (printer: 'a -> string) =
        let res = new System.Text.StringBuilder()
        let append (s : string) = res.Append s |> ignore
        append "[|"
        for i = 0 to arr.Length-1 do
            if i <> 0 then append "; "
            append (printer arr.[i])
        append "|]"
        res.ToString()

    let printList (list : list<'a>) (printer: 'a -> string) =
        let res = new System.Text.StringBuilder()
        let append (s : string) = res.Append s |> ignore
        append "["
        list |> List.iteri
            (fun i x ->
                if i <> 0 then append "; "
                append (printer x))
        append "]"
        res.ToString()

    let aboveStringListL = List.map wordL >> aboveListL

    (*let declareNonTermsArrays = 
        wordL ("let " + ruleName + " = Array.zeroCreate " + rules.rulesCount.ToString())
        @@ wordL ("let " + concatsName + " = Array.zeroCreate " + args.Length.ToString())*)


    let toStr (x : int) = x.ToString()
    let defineEpsilonTrees =
        let printChild (prod, arr) = "(" + toStr prod + "," + printArr arr toStr + ")"
        let printAst =
            function
            | Term _ -> failwith "Term was not expected in epsilon tree"
            | NonTerm (list,eps) ->
                "NonTerm (ref " + printList !list printChild + ", ref " + eps.Value.ToString() + ")"
        "let " + epsilonName + " : Tree<Token>[] = " +
            printArr grammar.epsilonTrees
                (function
                 | null -> "null"
                 | tree -> "new Tree<_>(" + printArr tree.Nodes printAst + ",0)")
        |> wordL

    // Realise rules
    let rec getProductionLayout num = function
        | PRef (name, args) ->
            incr num
            let name = Source.toString name
            let value = sprintf "((unbox %s.[%d]) : '_rnglr_type_%s) " childrenName !num name
            value + (printArgsCallOpt args)
            |> wordL
        | PToken name -> 
            incr num
            let name = Source.toString name
            sprintf "(match ((unbox %s.[%d]) : Token) with %s _rnglr_val -> [_rnglr_val] | a -> failwith \"%s expected, but %%A found\" a )"
                childrenName !num name name
            |> wordL
        | PSeq (s, ac) ->
            match ac with
            | None -> wordL "[]"
            | Some ac ->
                let actionCodeLayout =
                    (Source.toString ac).Split([|'\r'; '\n'|])
                    |> Array.filter ((<>) "")
                    |> List.ofArray
                    |> List.map wordL
                    |> aboveListL
                s
                |> List.map
                    (fun e ->
                        let var = 
                            if e.binding.IsNone || e.omit then (sprintf "_rnglr_var_%d" <| !num + 1)
                            else Source.toString e.binding.Value
                        let prod = getProductionLayout num e.rule
                        prod
                        -- wordL ("|> List.iter (fun (" + var + ") -> ")
                        //-- wordL (" do")
                    )
                |> List.rev
                |> List.fold
                    (fun acc x -> x @@-- acc -- wordL ")")
                    (wordL (resCycleName + " := (") -- actionCodeLayout -- wordL (")::!" + resCycleName))
                |> (fun x -> [wordL <| "let " + resCycleName + " = ref []"
                              x
                              wordL <| "!" + resCycleName
                             ] |> aboveListL)
                |> (fun x -> (wordL "(" @@-- x) @@ wordL ")")
        | x -> failwithf "unexpected construction: %A" x

    let getRuleLayout (rule : Rule.t<Source.t,Source.t>) i =
        let nonTermName = indexator.indexToNonTerm (rules.leftSide i)
        wordL (sprintf "fun (%s : array<_>) -> " childrenName)
        @@-- (wordL "box ("
              @@-- (wordL "(" ++ printArgsDeclare rule.args
                    @@-- getProductionLayout (ref -1) rule.body
                    -- wordL (") : '_rnglr_type_" + nonTermName + ")" )
                    )
             )

    let aboveArrayL = List.ofArray >> aboveListL
    let concats =
        let getConcat i = 
            let typeName = "'_rnglr_type_" + indexator.indexToNonTerm i
            let called = printArgsCallList args.[i] ""
            let args = printArgsDeclare args.[i]
            let listName = "_rnglr_list"
            let itemName = "_rnglr_item"
            wordL ("(fun (" + listName + " : list<_>) -> ")
            @@-- (wordL "box (" -- args)
            @@-- (wordL <| listName + " |> List.map (fun " + itemName
                            + " -> ((unbox " + itemName + ") : " + typeName + ") " + called
                            + " ) |> List.concat));")
        [|for i = 0 to args.Length - 1 do 
            yield getConcat i|]
        |> aboveArrayL

    let funRes =
        let funHead = wordL ("fun (tree : Tree<_>) -> ")
        let body = 
            let typeName = "'_rnglr_type_" + indexator.indexToNonTerm (grammar.rules.leftSide grammar.startRule)
            "unbox (tree.Translate " + ruleName + " " + " leftSide " + concatsName + " " + epsilonName + ") : " + typeName
            |> wordL
        funHead @@-- body

    let rules =
        wordL ("let translate = ")
        @@--
         (wordL ("let " + ruleName + ", " + concatsName + " = ")
          @@--
              (wordL "[|"
               @@
               (srcGrammar
                |> Array.mapi
                    (fun i rule ->
                        wordL "("
                        @@-- getRuleLayout rule i
                        @@-- wordL ");")
                |> aboveArrayL)
                @@ (wordL "|] , [|"
                    @@-- concats)
                @@ wordL "|] "))
           @@ funRes

    let nowarn = wordL "#nowarn \"64\";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type"

    [nowarn; defineEpsilonTrees; (*declareNonTermsArrays;*) rules]
    |> aboveListL
    |> Display.layout_to_string(FormatOptions.Default)
    |> out.WriteLine
    