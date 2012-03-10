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

let printTables (grammar : FinalGrammar) (tables : Tables) head (srcFileName : string) (tokenTypeOpt : string option) =
    use out = new System.IO.StreamWriter (srcFileName + ".ast.fs")
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
(*
    let print2DArr (arr : 'a[,]) checker printer
            name initValue conv =
        printInd 1 "let small_%s =\n" name
        printInd 2 "[|"
        for i = 0 to statesLim do
            if i <> 0 then printInd 2 " ;"
            print "[|"
            let mutable first = true
            for j = 0 to symbolsLim do
                if checker arr.[i,j] then
                    if not first then print "; "
                    else first <- false
                    print "%d," j
                    printer arr.[i,j]
            print "|]\n"
        printInd 2 "|]\n"
        printInd 1 "let %s = Array.zeroCreate %d\n" name statesCount 
        printInd 1 "for i = 0 to %d do\n" statesLim
        printInd 2 "%s.[i] <- Array.create %d %s\n" name symbolsCount initValue
        printInd 2 "for (x,y) in small_%s.[i] do\n" name
        printInd 3 "%s.[i].[x] <- %s y\n" name conv
*)
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

    let print2DArr (arr : 'a[,]) checker printer
            name initValue conv =
        printInd 1 "let small_%s =\n" name
        printInd 2 "[|"
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
            if not firstJ then print "|]"
        print "|]\n"
        printInd 1 "let %s = Array.zeroCreate %d\n" name statesCount 
        printInd 1 "for i = 0 to %d do\n" statesLim
        printInd 2 "%s.[i] <- Array.create %d %s\n" name symbolsCount initValue

        printInd 1 "for (i,t) in small_%s do\n" name
        printInd 2 "for (j,x) in t do\n"
        printInd 3 "%s.[i].[j] <- %s x\n" name conv

    let print2DArrList (arr : 'a list[,]) checker printer
            name initValue conv =
        printInd 1 "let lists_%s = " name
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
        printInd 1 "let small_%s =\n" name
        printInd 2 "[|"
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
        print "|]\n"
        printInd 1 "let %s = Array.zeroCreate %d\n" name statesCount 
        printInd 1 "for i = 0 to %d do\n" statesLim
        printInd 2 "%s.[i] <- Array.create %d %s\n" name symbolsCount initValue

        printInd 1 "let init_%s =\n" name
        printInd 2 "let mutable cur = 0\n"
        printInd 2 "while cur < small_%s.Length do\n" name
        printInd 3 "let i,length = unpack small_%s.[cur]\n" name
        printInd 3 "cur <- cur + 1\n"
        printInd 3 "for k = 0 to length-1 do\n"
        printInd 4 "let j,x = unpack small_%s.[cur + k]\n" name
        printInd 4 "%s.[i].[j] <- %s lists_%s.[x]\n" name conv name
        printInd 3 "cur <- cur + length\n"

    print "module RNGLR.Parse\n"
    print "open Yard.Generators.RNGLR.Parser\n"
    print "open Yard.Generators.RNGLR\n"

    match head with
    | None -> ()
    | Some (s : Source.t) ->
        print "%s" (Source.toString s)
        print "\n"

    print "type Token%s =\n"
    <|  match tokenTypeOpt with
        | None -> "<'a>"
        | Some _ -> ""
    let indexator = grammar.indexator
    for i = indexator.termsStart to indexator.termsEnd do
        printInd 2 "| %s of %s\n" (indexator.indexToTerm i)
        <|  match tokenTypeOpt with
            | None -> "'a"
            | Some s -> s

    print "\n"
    print "let buildAst<'a> =\n"
    printInd 1 "let inline unpack x = x >>> 16, x <<< 16 >>> 16\n"

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
        (fun l -> printList l (fun (x,y) -> print "%d,%d" x y))
        "reduces" "[]" ""

    print2DArrList zeroReduces
        (fun l -> not l.IsEmpty)
        (fun l -> printList l (fun (x,y) -> print "%d" x))
        "zeroReduces" "[]" ""

    printInd 1 "let small_acc = "
    printList tables.acc (fun x -> print "%d" x)
    print "\n"
    printInd 1 "let accStates = Array.zeroCreate %d\n" <| tables.gotos.GetLength 0
    printInd 1 "for i = 0 to %d do\n" statesLim
    printInd 2 "accStates.[i] <- List.exists ((=) i) small_acc\n"

    let rulesArr = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        rulesArr.[i] <- grammar.rules.rightSide i

    let totalRulesLength = rulesArr |> Array.sumBy (fun x -> x.Length)
    let rules = Array.zeroCreate totalRulesLength
    let rulesStart = Array.zeroCreate grammar.rules.rulesCount
    let mutable cur = 0
    for i = 0 to grammar.rules.rulesCount-1 do
        rulesStart.[i] <- cur
        for j = 0 to rulesArr.[i].Length-1 do
            rules.[cur] <- rulesArr.[i].[j]
            cur <- cur + 1
    printInd 1 "let rules = "
    printArr rules (print "%d")
    printInd 1 "let rulesStart = "
    printArr rulesStart (print "%d")

    printInd 1 "let leftSide =\n"
    printInd 2 ""
    let leftSide = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        leftSide.[i] <- grammar.rules.leftSide i
    printArr leftSide (print "%d")

    printInd 1 "let startRule = %d\n" grammar.startRule
    printInd 1 "let eofIndex = %d\n" grammar.indexator.eofIndex
    printInd 1 "let tokenToNumber = function\n"
    for i = indexator.termsStart to indexator.termsEnd do
        printInd 2 "| %s _ -> %d\n" (indexator.indexToTerm i) i

    printInd 1 "let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber)\n"
    printInd 1 "buildAst<_> parserSource"
    
    out.Close()