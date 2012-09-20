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

module Yard.Generators.RNGLR.Printer

open Yard.Generators.RNGLR.FinalGrammar
open System.Collections.Generic
open Yard.Generators.RNGLR
open Yard.Core.IL

let printTables (grammar : FinalGrammar) head (tables : Tables) (moduleName : string) (tokenType : string) (res : System.Text.StringBuilder) =
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

    let statesLim = tables.gotos.GetLength 0 - 1
    let symbolsLim = tables.gotos.GetLength 1 - 1
    let statesCount = statesLim + 1
    let symbolsCount = symbolsLim + 1

    let printArr (arr : 'a[]) printer =
        print "[|"
        for i = 0 to arr.Length-1 do
            if i <> 0 then print "; "
            printer arr.[i]
        printBr "|]"
    let pack x y = (x <<< 16) ||| y
    let printList l printer = 
        print "["
        l |> List.iteri (fun i x -> if i <> 0 then print "; "
                                    printer x)
        print "]"

    let printListAsArray l printer = 
        print "[|"
        l |> List.iteri (fun i x -> if i <> 0 then print "; "
                                    printer x)
        print "|]"

    let print2DArr (arr : 'a[,]) checker printer name =
        printBrInd 0 "let private small_%s =" name
        printInd 2 "[|"
        let mutable next = 1000
        let mutable cur = 0
        let mutable firstI = true
        for i = 0 to statesLim do
            let mutable firstJ = true
            for j = 0 to symbolsLim do
                if checker arr.[i,j] then
                    if not firstJ then print "; "
                    else
                        if not firstI then print "; "
                        else firstI <- false
                        print "%d, [|" i
                        firstJ <- false
                    print "%d," j
                    printer arr.[i,j]
                    cur <- cur + 1
            if not firstJ then print "|]"
            if cur > next then
                next <- next + 1000
                printBr ""
                printInd 10 ""
        printBr "|]"
        printBrInd 0 "let private %s = Array.zeroCreate %d" name statesCount 
        printBrInd 0 "for i = 0 to %d do" statesLim
        printBrInd 2 "%s.[i] <- Array.zeroCreate %d" name symbolsCount 

        printBrInd 0 "for (i,t) in small_%s do" name
        printBrInd 1 "for (j,x) in t do"
        printBrInd 2 "%s.[i].[j] <- x" name

    let print2DArrList (arr : 'a list[,]) checker printer name =
        printInd 0 "let private lists_%s = " name
        let lists = new Dictionary<_,_>()
        let next =
            let num = ref -1
            fun () -> incr num; !num
        for i = 0 to statesLim do
            for j = 0 to symbolsLim do
                if checker arr.[i,j] && not <| lists.ContainsKey arr.[i,j] then
                    lists.Add (arr.[i,j], next())
        let listsArr = Array.zeroCreate lists.Count
        for v in lists do
            listsArr.[v.Value] <- v.Key
        printArr listsArr printer
        printBrInd 0 "let private small_%s =" name
        printInd 2 "[|"
        let mutable next = 1000
        let mutable cur = 0
        let mutable firstI = true
        for i = 0 to statesLim do
            let mutable firstJ = true
            let good = new ResizeArray<_>(symbolsLim)
            for j = 0 to symbolsLim do
                 if checker arr.[i,j] then
                    good.Add j
            if good.Count > 0 then
                if not firstI then print "; "
                else firstI <- false

                print "%d" <| pack i good.Count
                for v = 0 to good.Count - 1 do
                    let j = good.[v]
                    print "; "
                    print "%d" <| pack j (lists.[arr.[i,j]])
                    cur <- cur + 1
                    if cur > next then
                        next <- next + 1000
                        printBr ""
                        printInd 10 ""
        printBr "|]"
        printBrInd 0 "let %s = Array.zeroCreate %d" name statesCount 
        printBrInd 0 "for i = 0 to %d do" statesLim
        printBrInd 2 "%s.[i] <- Array.zeroCreate %d" name symbolsCount

        printBrInd 0 "cur <- 0"
        printBrInd 0 "while cur < small_%s.Length do" name
        printBrInd 1 "let i = small_%s.[cur] >>> 16" name
        printBrInd 1 "let length = small_%s.[cur] &&& %d" name andNum
        printBrInd 1 "cur <- cur + 1"
        printBrInd 1 "for k = 0 to length-1 do"
        printBrInd 2 "let j = small_%s.[cur + k] >>> 16" name
        printBrInd 2 "let x = small_%s.[cur + k] &&& %d" name andNum
        printBrInd 2 "%s.[i].[j] <- lists_%s.[x]" name name
        printBrInd 1 "cur <- cur + length"

    printBr "type Token%s ="
    <|  match tokenType with
        | "" -> "<'a>"
        | _ -> ""
    let indexator = grammar.indexator
    for i = indexator.termsStart to indexator.termsEnd do
        printBrInd 1 "| %s of %s" (indexator.indexToTerm i)
        <|  match tokenType with
            | "" -> "'a"
            | s -> s

    printBr ""
    printBr "let numToString = function"

    for i = 0 to indexator.nonTermCount - 1 do
        printBrInd 1 "| %d -> \"%s\"" i (indexator.indexToNonTerm i)

    for i = indexator.termsStart to indexator.termsEnd do
        printBrInd 1 "| %d -> \"%s\"" i (indexator.indexToTerm i)

    printBrInd 1 "| _ -> \"\""

    printBrInd 0 "let tokenToNumber = function"
    for i = indexator.termsStart to indexator.termsEnd do
        printBrInd 1 "| %s _ -> %d" (indexator.indexToTerm i) i
    printBr ""

    printBr "let mutable private cur = 0"

    print "let leftSide = "
    let leftSide = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        leftSide.[i] <- grammar.rules.leftSide i
    printArr leftSide (print "%d")

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
    print "let private rules = "
    printArr rules (print "%d")

    print "let private rulesStart = "
    printArr rulesStart (print "%d")

    printBr "let startRule = %d" grammar.startRule
    printBr ""

    printBr "let acceptEmptyInput = %A" grammar.canInferEpsilon.[leftSide.[grammar.startRule]]
    printBr ""

    printBr "let defaultAstToDot ="
    printBrInd 1 "(fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)"

    printBr ""

    print2DArrList tables.gotos
        (fun x -> not x.IsEmpty)
        (fun x -> print "%d" x.[0])
        "gotos"
    
    let reduces,zeroReduces =
        let res = tables.reduces |> Array2D.map (List.partition (fun (_,x) -> x > 0))
        res |> Array2D.map fst
        , res |> Array2D.map snd

    print2DArrList reduces
        (fun l -> not l.IsEmpty)
        (fun l -> printListAsArray l (fun (x,y) -> print "%d,%d" x y))
        "reduces"

    print2DArrList zeroReduces
        (fun l -> not l.IsEmpty)
        (fun l -> printListAsArray l (fun (x,y) -> print "%d" x))
        "zeroReduces"

    printInd 0 "let private small_acc = "
    printList tables.acc (fun x -> print "%d" x)
    printBr ""
    printBrInd 0 "let private accStates = Array.zeroCreate %d" <| tables.gotos.GetLength 0
    printBrInd 0 "for i = 0 to %d do" statesLim
    printBrInd 2 "accStates.[i] <- List.exists ((=) i) small_acc"

    printBrInd 0 "let eofIndex = %d" grammar.indexator.eofIndex

    printBrInd 0 "let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)"

    printBr "let buildAst : (seq<Token> -> ParseResult<Token>) ="
    printBrInd 1 "buildAst<Token> parserSource"
    printBr ""
    res.ToString()
