module Yard.Generators.GLL2.PrintTable

open Yard.Generators.Common.FinalGrammar
open Yard.Generators.GLL
open System.Collections.Generic
open Yard.Core.IL
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let printTableGLL 
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
    let inline printBr2 (x : 'a) =
        Printf.kprintf (fun s -> res.Append(s) |> ignore) x
    let inline printBrInd num (x : 'a) =
         print "%s" (String.replicate (num <<< 2) " ")
         printBr x  

    let printArr prefix lBr rBr sep (arr : 'a[]) printer =
            print prefix
            print lBr
            for i = 0 to arr.Length-1 do
                if i <> 0 then print sep
                printer arr.[i]
            printBr rBr   
    
    let printArr2 prefix lBr rBr sep (arr : 'a[]) printer =
            print prefix
            print lBr
            for i = 0 to arr.Length-1 do
                if i <> 0 then print sep
                printer arr.[i]
            printBr2 rBr    

    let printList prefix lBr rBr sep l printer = 
         print prefix
         print lBr
         l |> List.iteri (fun i x -> if i <> 0 then print sep printer x)
         print rBr



    let printArr (arr : 'a[]) printer = printArr "" "[|" "|]" "; " (arr : 'a[]) printer
    let printArr2 (arr : 'a[]) printer = printArr2 "" "[|" "|]" "; " (arr : 'a[]) printer

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


    let printTable () =
        let indexator = grammar.indexator
        printBr "type Token ="
        let indexator = grammar.indexator
        let defaultType = tokenType.TryFind "_"

        for i = indexator.termsStart to indexator.termsEnd do
            let name = indexator.indexToTerm i
            let type' =
                match tokenType.TryFind name with
                | Some t -> t
                | None ->
                    match defaultType with
                    | Some t -> t
                    | None -> failwithf "Type of token %s in not defined" name
            printBrInd 1 "| %s%s" name 
            <|  match type' with
                | None -> ""
                | Some s -> " of (" + s + ")"

        let literalType = 
            match defaultType with
            | Some (Some t) -> t
            | _ -> failwithf "Default token type is not defined"

        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| L_%s of (%s)" (indexator.getLiteralName i) literalType

        let escapeQuotes = String.collect (function '"' -> "\\\"" | c -> string c)

        printBr ""
        let escapeQuotes = String.collect (function '"' -> "\\\"" | c -> string c)

        printBr ""
        printBr "let genLiteral (str : string) (data : %s) =" literalType
        if caseSensitive then "str"
        else "str.ToLower()"
        |> printBrInd 1 "match %s with"
            
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| \"%s\" -> Some (L_%s data)" (escapeQuotes <| indexator.indexToLiteral i) (indexator.getLiteralName i)
        printBrInd 1 "| x -> None"

        printBr "let tokenData = function"

        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s x -> box x" (indexator.indexToTerm i)

        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| L_%s x -> box x" (indexator.getLiteralName i)

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
            printBrInd 1 "| L_%s _ -> %d" (indexator.indexToLiteral i) i
        printBr ""

        printBrInd 0 "let isLiteral = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> false" <| indexator.indexToTerm i
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| L_%s _ -> true" (indexator.getLiteralName i)

        printBr ""

        printBrInd 0 "let isTerminal = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> true" <| indexator.indexToTerm i
        printBrInd 1 "| _ -> false"
        printBr ""

        printBrInd 0 "let numIsTerminal = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %d -> true" <| i
        printBrInd 1 "| _ -> false"
        printBr ""

        printBrInd 0 "let numIsNonTerminal = function"
        for i = 0 to indexator.nonTermCount - 1 do
            printBrInd 1 "| %d -> true" <| i
        printBrInd 1 "| _ -> false"
        printBr ""

        printBrInd 0 "let numIsLiteral = function"
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| %d -> true" <| i
        printBrInd 1 "| _ -> false"
        printBr ""

        printBrInd 0 "let isNonTerminal = function"   
        for i = 0 to indexator.nonTermCount-1 do
            printBrInd 1 "| %s -> true" <| indexator.indexToNonTerm i
        printBrInd 1 "| _ -> false"
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

        print "let table = [| "
        for arr in table.result do
            printArr2 arr (print "%s")
            print ";"
        print " |]"
        printBr ""

        print "let private rules = "
        printArr rules (print "%d")

        print "let private canInferEpsilon = "
        printArr grammar.canInferEpsilon (print "%A")

        printBr "let defaultAstToDot ="
        printBrInd 1 "(fun (tree : Yard.Generators.Common.AST2.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)"

        printBr ""

        print "let private rulesStart = "
        printArr rulesStart (print "%d")
        
        printBr "let startRule = %d" grammar.startRule
        printBr "let indexatorFullCount = %d" indexator.fullCount
        printBr "let rulesCount = %d" grammar.rules.rulesCount
        printBr "let indexEOF = %d" grammar.indexator.eofIndex
        printBr "let nonTermCount = %d" grammar.indexator.nonTermCount
        printBr "let termCount = %d" grammar.indexator.termCount
        printBr "let termStart = %d" grammar.indexator.termsStart
        printBr "let termEnd = %d" grammar.indexator.termsEnd
        printBr "let literalStart = %d" grammar.indexator.literalsStart
        printBr "let literalEnd = %d" grammar.indexator.literalsEnd
        printBr "let literalsCount = %d" grammar.indexator.literalsCount
        printBr "let resultAST = ref None"
        
        printBr ""
        printBr ""
        printBrInd 0 "let private parserSource = new ParserSource2<Token> (tokenToNumber, genLiteral, numToString, tokenData, isLiteral, isTerminal, isNonTerminal, getLiteralNames, table, rules, rulesStart, leftSide, startRule, literalEnd, literalStart, termEnd, termStart, termCount, nonTermCount, literalsCount, indexEOF, rulesCount, indexatorFullCount, acceptEmptyInput, numIsTerminal, numIsNonTerminal, numIsLiteral, canInferEpsilon)"
       
        let rules = grammar.rules
        let functionsNames = new System.Collections.Generic.Dictionary<string, int> ()
        let startFunctionName = ref 0
        let numberOfAlternatives = 
            let result = Array.zeroCreate indexator.nonTermCount
            let mutable count = 0
            let mutable previous = rules.leftSide 0
            for nTerm in rules.leftSideArr do
                if previous = nTerm
                then count <- count + 1
                else
                    result.[previous] <- count
                    count <- 1
                    previous <- nTerm
            result.[previous] <- count
            result
         
        let genFunctions () =
            let parameters = "(currentLabel : int ref)(currentIndex : int ref) (currentGSSNode : int64<vertex> ref) (currentR : ExtensionTree ref) (currentN : ExtensionTree ref) (funs : Functions) (tokens : 'TokenType[])"
            let shift = ref 0
            let currentFunNumber = ref 0
            let mutable previousNonTerm = -1
            let startName = "L_yard_start_rule"

            let getNumber name =
                if functionsNames.ContainsKey(name)
                then functionsNames.[name]
                else
                    let res = !currentFunNumber
                    functionsNames.Add(name, res)
                    currentFunNumber := !currentFunNumber + 1
                    res
 
            let rec generateForAlternative ruleNumber functionName position =
                shift := 0
                printBrInd !shift "let %s %s = " functionName parameters
                shift := !shift + 1
                printBrInd !shift "let rule = %d" ruleNumber
                printBrInd !shift "let mutable position = 0"
                let rule = rules.rightSide ruleNumber
                let ruleLength = rule.Length
                let mutable condition = false 
                if ruleNumber = grammar.startRule && position = 1
                then
                    printBrInd !shift "let label = funs.PackLabel rule 1"
                    printBrInd !shift "currentN := funs.GetNodeP label !currentN !currentR"
                    printBrInd !shift "let curRight = !currentN"
                    printBrInd !shift "let key = funs.Pack rule (getLeftExtension curRight.extension) (getRightExtension curRight.extension)"
                    printBrInd !shift "let resTree = funs.FindTree curRight.tree (rule) key"
                    printBrInd !shift "if  key = funs.FinalExt"
                    printBrInd !shift "then resultAST := Some resTree"
                    printBr ""
                else 
//                    if position = ruleLength
//                    then
//                        printBrInd !shift "currentN := funs.GetNodeP !currentLabel !currentN !currentR"
//                        printBrInd !shift "funs.Pop !currentGSSNode !currentIndex resTree currentN.Value.extension"
//                        printBr ""
//                    else
                    for pos in position..ruleLength - 1 do
                        if pos = ruleLength - 1 then condition <- true 
                        if indexator.isNonTerm rule.[pos]
                        then
                            let returnFunName = functionName + "_" + (pos + 1).ToString()
                            let n = getNumber returnFunName
                            printBrInd !shift "currentGSSNode := funs.Create %d !currentGSSNode !currentIndex !currentN" n
                            let nonTermName = "L_" + indexator.indexToNonTerm rule.[pos]
                            let n = getNumber nonTermName
                            printBrInd 1 "funs.AddContext %d !currentIndex !currentGSSNode funs.DummyAst" n
                            printBr ""
                            generateForAlternative ruleNumber returnFunName (pos + 1)
                        else
                            if pos = 0
                            then
                                printBrInd !shift "currentN := funs.GetNodeT !currentIndex"
                            else
                                printBrInd !shift "currentIndex := !currentIndex + 1"
                                printBrInd !shift "position <- position + 1"
                                printBrInd !shift "if !currentIndex < funs.Length && parserSource.rules.[rule].[position] = parserSource.TokenToNumber tokens.[!currentIndex]"
                                printBrInd !shift "then"
                                shift := !shift + 1
                                printBrInd !shift "currentR := funs.GetNodeT !currentIndex"
                                printBrInd !shift "let label = funs.PackLabel rule position"
                                printBrInd !shift "currentN := funs.GetNodeP label !currentN !currentR"
                    if condition && ruleNumber <> grammar.startRule && position <> 1
                    then
                        printBrInd !shift "funs.Pop !currentGSSNode !currentIndex !currentN currentN.Value.extension"    
                    printBr ""
            
            let mutable numberOfAlternate = 1
            for i in 0..rules.rulesCount - 1 do
                shift := 0
                let currentNonTerm = rules.leftSide i
                let currentNonTermName = indexator.indexToNonTerm currentNonTerm
                if previousNonTerm <> currentNonTerm then
                    previousNonTerm <- currentNonTerm
                    numberOfAlternate <- 1
                    let currentFunName = "L_" + currentNonTermName
                    if currentFunName = startName then startFunctionName := getNumber currentFunName
                    printBrInd !shift "let %s %s = " currentFunName parameters
                    shift := !shift + 1
                    for a in 1..numberOfAlternatives.[currentNonTerm] do
                        let name = "L_" + currentNonTermName + "_" + a.ToString() 
                        let n = getNumber name
                        printBrInd !shift "if funs.Test %d \"%s\" then" currentNonTerm name  
                        shift := !shift + 1
                        printBrInd !shift "funs.AddContext %d !currentIndex !currentGSSNode funs.DummyAst" n
                        shift := !shift - 1
                    printBr ""
                else numberOfAlternate <- numberOfAlternate + 1
                let name = "L_" + currentNonTermName + "_" + (numberOfAlternate.ToString())
                if not <| functionsNames.ContainsKey(name)
                then
                    functionsNames.Add(name, !currentFunNumber)
                    currentFunNumber := !currentFunNumber + 1
                generateForAlternative i name 0
        genFunctions()

        let takeArray =
            let result = Array.zeroCreate functionsNames.Count
            for k in functionsNames do
                result.[k.Value] <- k.Key
            result
        printBrInd 0 "let startFunctionName = %d" !startFunctionName
        print "let functions = "
        printArr takeArray (print "%s")
               
        printBr "let buildAst tokens ="
        printBrInd 1 "new buildAst<Token>(parserSource, tokens, functions, startFunctionName, resultAST)"
        printBr ""
         
        res.ToString()
    printTable ()