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

module Yard.Generators.GLL.PrintTable

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.GLL
open System.Collections.Generic
open Yard.Core.IL


let printTable 
    (grammar : FinalGrammar )(table : Table) (moduleName : string) 
    (tokenType : Map<_,_>) (res : System.Text.StringBuilder) 
    _class positionType caseSensitive : string =

    let inline print (x : 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x
    let inline printInd num (x : 'a) =
         print "%s" (String.replicate (num <<< 2) " ")
         print x
    let inline printBr (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x
    let inline printBrInd num (x : 'a) =
         print "%s" (String.replicate (num <<< 2) " ")
         printBr x
    let andNum = (1 <<< 16) - 1

    
    let pack x y = (x <<< 16) ||| y    

    let printArr prefix lBr rBr sep (arr : 'a[]) printer =
            print prefix
            print lBr
            for i = 0 to arr.Length-1 do
                if i <> 0 then print sep
                printer arr.[i]
            printBr rBr        

    let printList prefix lBr rBr sep l printer = 
         print prefix
         print lBr
         l |> List.iteri (fun i x -> if i <> 0 then print sep printer x)
         print rBr



    let printArr (arr : 'a[]) printer = printArr "" "[|" "|]" "; " (arr : 'a[]) printer
   // let printListAsArray l printer = printList "" "[|" "|]" "; " l printer
   // let printList l printer = printList "" "[" "]" "; " l printer

    let leftSide = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        leftSide.[i] <- grammar.rules.leftSide i

    let rulesArr = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        rulesArr.[i] <- grammar.rules.rightSide i

    let totalRulesLength = rulesArr |> Array.sumBy (fun x -> x.Length)
    let rules = Array.zeroCreate totalRulesLength
    let rulesStart = Array.zeroCreate <| grammar.rules.rulesCount + 1
    let mutable cur = 0
    for i = 0 to grammar.rules.rulesCount-1 do
        rulesStart.[i] <- cur
        for j = 0 to rulesArr.[i].Length-1 do
            rules.[cur] <- rulesArr.[i].[j]
            cur <- cur + 1
    rulesStart.[grammar.rules.rulesCount] <- cur


    let printTableToFSharp () =
       
       // printBr "type Token ="
        let indexator = grammar.indexator
        //let defaultType = tokenType.TryFind "_"
        //for i = indexator.termsStart to indexator.termsEnd do
        //    let name = indexator.indexToTerm i
         //   let type' =
         //       match tokenType.TryFind name with
         //       | Some t -> t
         // //      | None ->
         //           match defaultType with
         //           | Some t -> t
         //           | None -> failwithf "Type of token %s in not defined" name
//
  //          printBrInd 1 "| %s%s" name 
    //        <|  match type' with
      //          | None -> ""
        //        | Some s -> " of (" + s + ")"

//        for i = indexator.literalsStart to indexator.literalsEnd do
  //          if positionType = "" then
    //            failwith "RNGLR: Unspecified position type"
      //      printBrInd 1 "| ``L %d`` of (%s * %s)" i positionType positionType

        let escapeQuotes = String.collect (function '"' -> "\\\"" | c -> string c)

        printBr ""
        printBr "let genLiteral (str : string) posStart posEnd ="
        if caseSensitive then "str"
        else "str.ToLower()"
        |> printBrInd 1 "match %s with"
            
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| \"%s\" -> ``L %d`` (posStart, posEnd)" (escapeQuotes <| indexator.indexToLiteral i) i
        printBrInd 1 "| x -> failwithf \"Literal %%s undefined\" x"
        //

        printBr "let tokenData = function"

        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s x -> box x" (indexator.indexToTerm i)

        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| ``L %d`` x -> box x" i

        printBr ""
        printBr "let numToString = function"

        for i = 0 to indexator.nonTermCount - 1 do
            printBrInd 1 "| %d -> \"%s\"" i (indexator.indexToNonTerm i)

        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %d -> \"%s\"" i (indexator.indexToTerm i)

        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| %d -> \"'%s'\"" i (escapeQuotes <| indexator.indexToLiteral i)

        printBrInd 1 "| _ -> \"\""
        printBr ""

        printBrInd 0 "let tokenToNumber = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> %d" (indexator.indexToTerm i) i
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| ``L %d`` _ -> %d" i i
        printBr ""

        printBrInd 0 "let isLiteral = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> false" <| indexator.indexToTerm i
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| ``L %d`` _ -> true" i
        printBr ""

        printInd 0 "let getLiteralNames = ["
        for i = indexator.literalsStart to indexator.literalsEnd do
            print "\"%s\";" <| indexator.indexToLiteral i
        print "]"
        printBr ""

        printBr "let mutable private cur = 0"

        print "let leftSide = "
        printArr leftSide (print "%d")

        print "let table = "
        printArr table.result (print "%d")

        print "let private rules = "
        printArr rules (print "%d")

        print "let private rulesStart = "
        printArr rulesStart (print "%d")

        printBr "let startRule = %d" grammar.startRule
        printBr ""

        printBr ""

        printBrInd 0 "let private parserSource = new ParserSource<Token> (table, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)"
        
        res.ToString()
    printTableToFSharp ()