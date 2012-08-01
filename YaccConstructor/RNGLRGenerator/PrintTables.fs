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

let printTables (grammar : FinalGrammar) head (tables : Tables)
        (out : System.IO.StreamWriter) (moduleName : string) (tokenType : string) =
    let tab = 4
    let print (x : 'a) =
        fprintf out x
    let printInd num (x : 'a) =
        print "%s" (String.replicate (tab * num) " ")
        print x

    let statesLim = tables.gotos.GetLength 0 - 1
    let symbolsLim = tables.gotos.GetLength 1 - 1
    let statesCount = statesLim + 1
    let symbolsCount = symbolsLim + 1

    let printArr (arr : 'a[]) printer =
        print "[|"
        for i = 0 to arr.Length-1 do
            if i <> 0 then print "; "
            printer arr.[i]
        print "|]\n"
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

    let print2DArr (arr : 'a[,]) checker printer
            name initValue conv =
        printInd 0 "let private small_%s =\n" name
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
                print "\n"
                printInd 10 ""
        print "|]\n"
        printInd 0 "let private %s = Array.zeroCreate %d\n" name statesCount 
        printInd 0 "for i = 0 to %d do\n" statesLim
        printInd 2 "%s.[i] <- Array.create %d %s\n" name symbolsCount initValue

        printInd 0 "for (i,t) in small_%s do\n" name
        printInd 2 "for (j,x) in t do\n"
        printInd 3 "%s.[i].[j] <- %s x\n" name conv

    let print2DArrList (arr : 'a list[,]) checker printer
            name initValue conv =
        printInd 0 "let private lists_%s = " name
        let lists = new Dictionary<_,_>()
        let next =
            let num = ref -1
            fun () -> incr num; !num
        for i = 0 to statesLim do
            for j = 0 to symbolsLim do
                if not <| lists.ContainsKey arr.[i,j] then
                    lists.Add (arr.[i,j], next())
        let listsArr = Array.zeroCreate lists.Count
        for v in lists do
            listsArr.[v.Value] <- v.Key
        printArr listsArr printer
        printInd 0 "let private small_%s =\n" name
        printInd 2 "[|"
        let mutable next = 1000
        let mutable cur = 0
        let mutable firstI = true
        for i = 0 to statesLim do
            let mutable firstJ = true
            let good = [for j = 0 to symbolsLim do
                            if checker arr.[i,j] then yield j]
            if good.Length > 0 then
                if not firstI then print "; "
                else firstI <- false

                print "%d" <| pack i good.Length
                for j in good do
                    print "; "
                    print "%d" <| pack j (lists.Item arr.[i,j])
                    cur <- cur + 1
                    if cur > next then
                        next <- next + 1000
                        print "\n"
                        printInd 10 ""
        print "|]\n"
        printInd 0 "let %s = Array.zeroCreate %d\n" name statesCount 
        printInd 0 "for i = 0 to %d do\n" statesLim
        printInd 2 "%s.[i] <- Array.create %d %s\n" name symbolsCount initValue

        printInd 0 "let init_%s =\n" name
        printInd 2 "let mutable cur = 0\n"
        printInd 2 "while cur < small_%s.Length do\n" name
        printInd 3 "let i,length = unpack small_%s.[cur]\n" name
        printInd 3 "cur <- cur + 1\n"
        printInd 3 "for k = 0 to length-1 do\n"
        printInd 4 "let j,x = unpack small_%s.[cur + k]\n" name
        printInd 4 "%s.[i].[j] <- %s lists_%s.[x]\n" name conv name
        printInd 3 "cur <- cur + length\n"

    print "type Token%s =\n"
    <|  match tokenType with
        | "" -> "<'a>"
        | _ -> ""
    let indexator = grammar.indexator
    for i = indexator.termsStart to indexator.termsEnd do
        printInd 1 "| %s of %s\n" (indexator.indexToTerm i)
        <|  match tokenType with
            | "" -> "'a"
            | s -> s

    print "\n"
    print "let numToString = function \n"

    for i = 0 to indexator.nonTermCount - 1 do
        printInd 1 "| %d -> \"%s\"\n" i (indexator.indexToNonTerm i)

    for i = indexator.termsStart to indexator.termsEnd do
        printInd 1 "| %d -> \"%s\"\n" i (indexator.indexToTerm i)

    printInd 1 "| _ -> \"\"\n"

    printInd 0 "let tokenToNumber = function\n"
    for i = indexator.termsStart to indexator.termsEnd do
        printInd 1 "| %s _ -> %d\n" (indexator.indexToTerm i) i
    print "\n"

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

    print "let startRule = %d\n" grammar.startRule
    print "\n"

    print "let defaultAstToDot = \n"
    printInd 1 "(fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)\n"

    print "\n"
    printInd 0 "let inline unpack x = x >>> 16, x <<< 16 >>> 16\n"

    print2DArr tables.gotos
        (fun x -> x.IsSome)
        (fun x -> print "%d" x.Value)
        "gotos" "None" "Some "
    
    let reduces,zeroReduces =
        let res = tables.reduces |> Array2D.map (List.partition (fun (_,x) -> x > 0))
        res |> Array2D.map fst
        , res |> Array2D.map snd

    print2DArrList reduces
        (fun l -> not l.IsEmpty)
        (fun l -> printListAsArray l (fun (x,y) -> print "%d,%d" x y))
        "reduces" "[||]" ""

    print2DArrList zeroReduces
        (fun l -> not l.IsEmpty)
        (fun l -> printListAsArray l (fun (x,y) -> print "%d" x))
        "zeroReduces" "[||]" ""

    printInd 0 "let private small_acc = "
    printList tables.acc (fun x -> print "%d" x)
    print "\n"
    printInd 0 "let private accStates = Array.zeroCreate %d\n" <| tables.gotos.GetLength 0
    printInd 0 "for i = 0 to %d do\n" statesLim
    printInd 2 "accStates.[i] <- List.exists ((=) i) small_acc\n"

    printInd 0 "let eofIndex = %d\n" grammar.indexator.eofIndex

    printInd 0 "let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)\n"

    print "let buildAst : (seq<Token> -> ParseResult<Token>) =\n"
    printInd 1 "buildAst<Token> parserSource\n\n"
    