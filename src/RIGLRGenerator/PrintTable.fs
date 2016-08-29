module Yard.Generators.RIGLRGenerator.PrintTable

open Yard.Generators.Common.FinalGrammar

let printTable 
    (grammar: FinalGrammar) table moduleName (tokenType: Map<_,_>) (finalState: int list)
    (popStates: Set<int>) (res: System.Text.StringBuilder) positionType caseSensitive isAbstract =

    let inline print (x: 'a) =
        Printf.kprintf (fun s -> res.Append s |> ignore) x
    let inline printInd num (x: 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        print x
    let inline printBr (x: 'a) =
        Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x
    let inline printBrInd num (x: 'a) =
        print "%s" (String.replicate (num <<< 2) " ")
        printBr x
    
    let printArr (arr: 'a[]) printer =        
        print "[|"
        for i = 0 to arr.Length - 1 do
            if i <> 0 then print "; "                               
            printer arr.[i]            
        printBr "|]"
    
    let printTableArr (table: 'a[]) printer =
        printInd 1 "[|"
        for i = 0 to table.Length - 1 do
            if i <> 0 
            then 
                print "; "
                if i % 5 = 0 
                then 
                    printBr ""
                    printInd 1 ""
                    print "  "                               
            printer table.[i]
        printBr "|]"
    
    let leftSide = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount - 1 do
        leftSide.[i] <- grammar.rules.leftSide i

    let rulesArr = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount - 1 do
        rulesArr.[i] <- grammar.rules.rightSide i

    let totalRulesLength = rulesArr |> Array.sumBy (fun x -> x.Length)
    let rules = Array.zeroCreate totalRulesLength
    let rulesStart = Array.zeroCreate <| grammar.rules.rulesCount + 1
    let mutable cur = 0
    for i = 0 to grammar.rules.rulesCount - 1 do
        rulesStart.[i] <- cur
        for j = 0 to rulesArr.[i].Length - 1 do
            rules.[cur] <- rulesArr.[i].[j]
            cur <- cur + 1
    rulesStart.[grammar.rules.rulesCount] <- cur

    let printTable () =
        let indexator = grammar.indexator
        let defaultType = tokenType.TryFind "_"
        printBr "type Token ="
        
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
        
        printBr ""
        printBr "let genLiteral (str : string) (data : %s) =" literalType
        if caseSensitive then "str"
        else "str.ToLower()"
        |> printBrInd 1 "match %s with"

        let escapeQuotes = String.collect (function '"' -> "\\\"" | c -> string c)

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
            printBrInd 1 "| L_%s _ -> %d" (indexator.getLiteralName i) i
        printBr ""
        
        printBr "let startRule = %A" grammar.startRule
        printBr "let eofIndex = %d" grammar.indexator.eofIndex
        print "let private rules = "
        printArr rules (print "%d")
        print "let private rulesStart = "
        printArr rulesStart (print "%d")
        printBr "let finalState = %A" finalState
        printBr "let popStates = set %A" (Seq.toArray popStates)
        printBr "let leftSide = %A" grammar.rules.leftSideArr
        let epsilonRules = new System.Collections.Generic.List<_>()
        for i in 0 .. grammar.rules.rulesCount - 1 do
            if (grammar.rules.rightSide i).Length = 0
            then epsilonRules.Add i 
        printBr "let epsilonRules = %A" <| epsilonRules.ToArray()
        printBr ""

        printBr "let table: (int*int)[][][] = "
        printTableArr table (fun e -> print "%A" e)      
        printBr ""
        
        printBr "let private parserSource = new ParserSource<Token> (table, tokenToNumber, genLiteral, numToString, tokenData, rules, rulesStart, leftSide, startRule, eofIndex, popStates, finalState)"
        printBr "let buildAst : (seq<Token> -> ParseResult<Token>) ="
        printBrInd 1 "buildAst<Token> parserSource"
        printBr ""
        
        res.ToString()

    printTable()