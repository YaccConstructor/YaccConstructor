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

    let printArr2 (arr : 'a[]) printer num = 
         print "%s" (String.replicate (num <<< 2) " ")
         printArr arr printer

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
       
        let indexator = grammar.indexator

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

        printBrInd 0 "let isTerminal = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> true" <| indexator.indexToTerm i
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| ``L %d`` _ -> false" i
        for i = 0 to indexator.nonTermCount-1 do
            printBrInd 1 "| %s _ -> false" <| indexator.indexToNonTerm i
        printBr ""

        printBrInd 0 "let isNonTerminal = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> false" <| indexator.indexToTerm i
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| ``L %d`` _ -> false" i
        for i = 0 to indexator.nonTermCount-1 do
            printBrInd 1 "| %s _ -> true" <| indexator.indexToNonTerm i
        printBr ""

        printInd 0 "let getLiteralNames = ["
        for i = indexator.literalsStart to indexator.literalsEnd do
            print "\"%s\";" <| indexator.indexToLiteral i
        print "]"
        printBr ""

        printBr "let mutable private cur = 0"
        printBr ""
        
        printBr "let acceptEmptyInput = %A" grammar.canInferEpsilon.[leftSide.[grammar.startRule]]
        printBr ""

        print "let leftSide = "
        printArr leftSide (print "%d")

        print "let table = "
        print "\n"
        printBrInd 1 "[| "
        for arrs in table.result do
            printArr2 arrs (print "%d") 2    
        printBrInd 1 " |] "

       

        print "let private rules = "
        printArr rules (print "%d")

        print "let private rulesStart = "
        printArr rulesStart (print "%d")

        printBr "let startRule = %d" grammar.startRule
        printBr "let indexatorFullCount = %d" indexator.fullCount
        printBr "let rulesCount = %d" grammar.rules.rulesCount
        printBr "let indexEOF = %d" grammar.indexator.eofIndex
        printBr "let literalsCount = %d" grammar.indexator.literalsCount
        printBr "let literalStart = %d" grammar.indexator.literalsStart
        printBr "let nonTermCount = %d" grammar.indexator.nonTermCount
        printBr "let termCount = %d" grammar.indexator.termCount
        printBr "let termStart = %d" grammar.indexator.termsStart
        printBr "let termEnd = %d" grammar.indexator.termsEnd
        printBr "let literalStart = %d" grammar.indexator.literalsStart
        printBr "let literalEnd = %d" grammar.indexator.literalsEnd
       

        printBr ""

        printBr ""

        printBrInd 0 "let private parserSource = 
            new ParserSource<Token> (
                               tokenToNumber        : 'TokenType -> int
                               , genLiteral         : string -> int -> int -> string
                               , numToString        : int -> string
                               , tokenData          : int -> string
                               , isLiteral          : string -> string
                               , isTerminal         : string -> string
                               , isNonTerminal      : string -> string
                               , getLiteralNames    : string -> string
                               , table              : int[][]
                               , rules              : int[][]
                               , rulesStart         : int[]
                               , leftSide           : int[]
                               , startRule          : int
                               , literalEnd         : int
                               , literalStart       : int
                               , termEnd            : int
                               , termStart          : int
                               , termCount          : int
                               , nonTermCount       : int
                               , literalCount       : int
                               , indexEOF           : int
                               , rulesCount         : int
                               , indexatorFullCount : int
                               , acceptEmptyInput   : bool)"
                       
    
        res.ToString()
    printTableToFSharp ()