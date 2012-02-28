//  Parser.fs contains type, describing information, written to file as result of generation
//     and used by Parser and Translator.
//
//  Copyright 2011-2012 Avdyukhin Dmitry
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

module Yard.Generators.RNGLR.Printer

open Yard.Generators.RNGLR.FinalGrammar
open Yard.Generators.RNGLR

let printTables (grammar : FinalGrammar) (tables : Tables) (srcFileName : string) =
    use out = new System.IO.StreamWriter (srcFileName + ".ast.fs")
    let tab = 4
    let print (x : 'a) =
        fprintf out x
    let printInd num (x : 'a) =
        print "%s" (String.replicate (tab * num) " ")
        print x
    let statesLim = tables.gotos.GetLength 0 - 1
    let symbolsLim = tables.gotos.GetLength 1 - 1
    let print2DArr (arr : 'a[,]) printer =
        printInd 2 "[|"
        for i = 0 to statesLim do
            if i <> 0 then printInd 2 " ;"
            print "[|"
            for j = 0 to symbolsLim do
                if j <> 0 then print "; "
                printer arr.[i,j]
            print "|]\n"
        printInd 2 "|]\n"

    let printArr (arr : 'a[]) printer =
        print "[|"
        for i = 0 to arr.Length-1 do
            if i <> 0 then print "; "
            printer arr.[i]
        print "|]\n"
    let printList l printer = 
        print "["
        l |> List.iteri (fun i x -> if i <> 0 then print "; "
                                                   printer x)
        print "]"
    print "module RNGLR.Parse\n"
    print "open Yard.Generators.RNGLR.Parser\n"
    print "open Yard.Generators.RNGLR\n"

    print "type Token<'a> =\n"
    let indexator = grammar.indexator
    for i = indexator.termsStart to indexator.termsEnd do
        printInd 2 "| %s of 'a\n" (indexator.indexToTerm i)

    print "\n"
    print "let buildAst<'a> =\n"

    printInd 1 "let gotos =\n"
    print2DArr tables.gotos
        (function
         | None -> print "None"
         | Some x -> print "Some %d" x)
    
    let reduces,zeroReduces =
        let res = tables.reduces |> Array2D.map (List.partition (fun (_,x) -> x > 0))
        res |> Array2D.map fst
        , res |> Array2D.map snd
    printInd 1 "let reduces =\n"
    print2DArr reduces
        (fun l -> printList l (fun (x,y) -> print "%d,%d" x y))

    printInd 1 "let zeroReduces =\n"
    print2DArr zeroReduces
        (fun l -> printList l (fun (x,y) -> print "%d,%d" x y))

    let accStates = Array.zeroCreate <| tables.gotos.GetLength 0
    for i = 0 to statesLim do
        accStates.[i] <- List.exists ((=) i) tables.acc

    printInd 1 "let accStates = "
    printArr accStates (print "%A")

    let rules = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        rules.[i] <- grammar.rules.rightSide i

    printInd 1 "let rules =\n"
    printInd 2 ""
    printArr rules
        (fun arr -> printArr arr (print "%d")
                    printInd 2 "")
    
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

    printInd 1 "let parserSource = new ParserSource<_> (gotos, reduces, zeroReduces, accStates, rules, leftSide, startRule, eofIndex, tokenToNumber)\n"
    printInd 1 "buildAst<_> parserSource"
    
    out.Close()