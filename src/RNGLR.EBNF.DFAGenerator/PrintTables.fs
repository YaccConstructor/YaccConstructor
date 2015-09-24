module Yard.Generators.RNGLR.EBNF.DFA.Printer

open Yard.EBNF.DFA.FinalGrammar
open System.Collections.Generic
open Yard.Generators.RNGLR.EBNF.DFA
open Yard.Generators.RNGLR.EBNF.DFA.States
open Yard.Core.IL


type TargetLanguage =
    | FSharp
    | Scala

let printTablesEBNF
    (grammar : FinalGrammarNFA) head (tables : TablesEBNF) (moduleName : string) 
    (tokenType : Map<_,_>) (res : System.Text.StringBuilder) targetLanguage 
    _class positionType caseSensitive =
    
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
        l |> List.iteri (fun i x -> if i <> 0 then print sep
                                    printer x)
        print rBr

    let printArrHashSet prefix lBr rBr sep (arr : HashSet<_> []) printer =
        print prefix
        print lBr
        for i = 0 to arr.Length - 1 do
            if i <> 0 then print sep
            print lBr
            let mutable firstEl = true
            for e in arr.[i] do
                if not firstEl then print sep
                else firstEl <- false
                printer e
            print rBr
        printBr rBr

    let print2DArrList lBr rBr sep (bindKW:string) printArr resultArrayCreationCodePrinter (arr : 'a list[,]) checker printer name =
        printInd 0 (Printf.StringFormat<_,_>(bindKW + " lists_%s = ")) name
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
        printInd 0 (Printf.StringFormat<_,_>(bindKW + " small_%s = ")) name
        print lBr
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
                if not firstI then print sep
                else firstI <- false

                print "%d" <| pack i good.Count
                for v = 0 to good.Count - 1 do
                    let j = good.[v]
                    print sep
                    print "%d" <| pack j lists.[arr.[i,j]]
                    cur <- cur + 1
                    if cur > next then
                        next <- next + 1000
                        printBr ""
                        printInd 10 ""
        printBr rBr

        resultArrayCreationCodePrinter name

    let print2DArrListDfa lBr rBr sep (bindKW:string) printArr resultArrayCreationCodePrinter (arr : 'a list[,]) checker printer name =
        let iCount = arr.GetLength 0 - 1
        let jCount = arr.GetLength 1 - 1

        printInd 0 (Printf.StringFormat<_,_>(bindKW + " lists_%s = ")) name
        let lists = new Dictionary<_,_>()
        let next =
            let num = ref -1
            fun () -> incr num; !num
        for i = 0 to iCount do
            for j = 0 to jCount do
                if checker arr.[i,j] && not <| lists.ContainsKey arr.[i,j] then
                    lists.Add (arr.[i,j], next())
        let listsArr = Array.zeroCreate lists.Count
        for v in lists do
            listsArr.[v.Value] <- v.Key
        printArr listsArr printer
        printInd 0 (Printf.StringFormat<_,_>(bindKW + " small_%s = ")) name
        print lBr
        let mutable next = 1000
        let mutable cur = 0
        let mutable firstI = true
        for i = 0 to iCount do
            let mutable firstJ = true
            let good = new ResizeArray<_>(symbolsLim)
            for j = 0 to jCount do
                    if checker arr.[i,j] 
                    then
                        good.Add j
            if good.Count > 0 
            then
                if not firstI 
                then print sep
                else firstI <- false

                print "%d" <| pack i good.Count
                for v = 0 to good.Count - 1 do
                    let j = good.[v]
                    print sep
                    print "%d" <| pack j lists.[arr.[i,j]]
                    cur <- cur + 1
                    if cur > next then
                        next <- next + 1000
                        printBr ""
                        printInd 10 ""
        printBr rBr
        resultArrayCreationCodePrinter name (iCount + 1)


    let leftSide = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        leftSide.[i] <- grammar.rules.leftSide i

    let rulesArr = Array.zeroCreate grammar.rules.rulesCount
    for i = 0 to grammar.rules.rulesCount-1 do
        rulesArr.[i] <- grammar.rules.rightSide i

    let rulesCount = grammar.reverseRules.rulesCount
    let dfaTableArr = Array.zeroCreate rulesCount
    for i = 0 to rulesCount - 1 do
        dfaTableArr.[i] <- grammar.reverseRules.dfaTable i
    let dfaArr = dfaTableArr |> Array.map (fun x -> fst x)
    let dfaArrList = Array2D.create rulesCount grammar.indexator.fullCount []
    for i = 0 to rulesCount - 1 do
        for j = 0 to grammar.indexator.fullCount - 1 do
            for s = 0 to dfaArr.[i].Length - 1 do
                dfaArrList.[i, j] <- s::(dfaArr.[i].[s].[j])::dfaArrList.[i, j]
    let finitStatesSet = dfaTableArr |> Array.map (fun x -> snd x)
    let amountOfStates = Array.zeroCreate rulesCount
    for i = 0 to rulesCount - 1 do
        amountOfStates.[i] <- dfaArr.[i].Length
    

    let totalRulesNumberOfStates = rulesArr |> Array.sumBy (fun x -> x.numberOfStates)
    (*let rulesStart = Array.zeroCreate <| grammar.rules.rulesCount + 1
    let mutable cur = 0
    for i = 0 to grammar.rules.rulesCount-1 do
        rulesStart.[i] <- cur
        for j = 0 to rulesArr.[i].Length-1 do
            rules.[cur] <- rulesArr.[i].[j]
            cur <- cur + 1
    rulesStart.[grammar.rules.rulesCount] <- cur*)

    let reduces,zeroReduces =
        let isReduce (label : ReduceLabel) =
            match label with
                |Reduce -> true
                |StackReduce -> false
        let res = tables.reduces |> Array2D.map (List.partition (fun (_,x) -> isReduce x))
        res |> Array2D.map fst |> Array2D.map (List.map (fun (x,_) -> x))
        , res |> Array2D.map snd |> Array2D.map (List.map (fun (x,_) -> x))

    let printTablesToFSharp () =
        let printArr (arr : 'a[]) printer = printArr "" "[|" "|]" "; " (arr : 'a[]) printer
        let printListAsArray l printer = printList "" "[|" "|]" "; " l printer
        let printSetAsArray s printer = printList "" "[|" "|]" "; " (Set.toList s) printer
        let printList l printer = printList "" "[" "]" "; " l printer
        let printArrHashSet prefix hashSet printer = printArrHashSet prefix "[|" "|]" "; " hashSet printer

        let resultReducesArrayCreationCodePrinter name = 
            printBrInd 0 "let %s = Array.zeroCreate %d" name statesCount 
            printBrInd 0 "for i = 0 to %d do" statesLim
            printBrInd 1 "%s.[i] <- Array.zeroCreate %d" name symbolsCount

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

        let resultGotoArrayCreationCodePrinter() =
            let name = "gotos"
            printBrInd 0 "let %s = Array.zeroCreate %d" name statesCount 
            printBrInd 0 "for i = 0 to %d do" statesLim
            printBrInd 1 "%s.[i] <- Array.create %d None" name symbolsCount

            printBrInd 0 "cur <- 0"
            printBrInd 0 "while cur < small_%s.Length do" name
            printBrInd 1 "let i = small_%s.[cur] >>> 16" name
            printBrInd 1 "let length = small_%s.[cur] &&& %d" name andNum
            printBrInd 1 "cur <- cur + 1"
            printBrInd 1 "for k = 0 to length-1 do"
            printBrInd 2 "let j = small_%s.[cur + k] >>> 16" name
            printBrInd 2 "let x = small_%s.[cur + k] &&& %d" name andNum
            
//            printBrInd 2 "let dontStackSetNum = small_%s.[cur + k*2 + 1] >>> 16" name
//            printBrInd 2 "let stackSetNum = small_%s.[cur + k*2 + 1] &&& %d" name andNum

            printBrInd 2 "%s.[i].[j] <- Some (lists_%s.[x])" name name
//            printBrInd 2 "%s.[i].[j] <- Some (lists_%s.[x], (dontStackSetNum, stackSetNum))" name name
            printBrInd 1 "cur <- cur + length"


        let resultDfaArrayCreationCodePrinter name prodCount = 
            printBrInd 0 "let %s = Array.zeroCreate %d" name prodCount
            printBrInd 0 "for i = 0 to %d do" (prodCount - 1)
            printBrInd 1 "%s.[i] <- Array.zeroCreate %d" name symbolsCount
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
                
        let printGotoArrList() =
            // although we have lists of gotos and stacLabels, only first elements of these lists are really processed
            let checker = fun (x : _ list ) -> not x.IsEmpty
            let printerGotos = fun (x : _ list ) -> print "%d" x.[0]
            //let printersStackSets = fun x -> printSetAsArray x (fun y -> print "%d" y)
            let name = "gotos"
            let lBr : Printf.StringFormat<_,_> =  "[|"
            let rBr : Printf.StringFormat<_,_> =  "|]"
            let sep : Printf.StringFormat<_,_> =  "; "
            let bindKW = "let private"
            

            let gotos = tables.gotos

            (*let stackLabelSign = function
            |DontStack -> 0
            |Stack _ -> 1
            |StackingConflict _-> 2*)
                        
            let gotoLists = new Dictionary<_,_>()
            //let stackSetLists = new Dictionary<_,_>()
            let nextGoto =
                let numGoto = ref -1
                fun () -> incr numGoto; !numGoto
//            let nextGoto, nextStackSet =
//                let numGoto = ref -1
//                let numSet = ref -1
//                (fun () -> incr numGoto; !numGoto),
//                (fun () -> incr numSet; !numSet)
            for i = 0 to statesLim do
                for j = 0 to symbolsLim do
                    if checker gotos.[i,j] then
                        if not <| gotoLists.ContainsKey gotos.[i,j] then
                            gotoLists.Add (gotos.[i,j], nextGoto())
                        //let dontStackSet, stackSet = stackSets.[i,j].[0]
//                        if not <| stackSetLists.ContainsKey dontStackSet then
//                                stackSetLists.Add(dontStackSet, nextStackSet())
//                        if not <| stackSetLists.ContainsKey stackSet then
//                                stackSetLists.Add(stackSet, nextStackSet())
            let gotoListsArr = Array.zeroCreate gotoLists.Count
            for v in gotoLists do
                gotoListsArr.[v.Value] <- v.Key
//            let stackSetListsArr = Array.zeroCreate stackSetLists.Count
//            for v in stackSetLists do
//                stackSetListsArr.[v.Value] <- v.Key
            printInd 0 (Printf.StringFormat<_,_>(bindKW + " lists_%s = ")) name
            printArr gotoListsArr printerGotos
//            printInd 0 (Printf.StringFormat<_,_>(bindKW + " %s = ")) "stackArrays"
//            printArr stackSetListsArr printersStackSets
//            printBrInd 0 (Printf.StringFormat<_,_>(bindKW + " %s = stackArrays |> Array.map Set.ofArray")) "stackSets"

            printInd 0 (Printf.StringFormat<_,_>(bindKW + " small_%s = ")) name
            print lBr
            let mutable next = 1000
            let mutable cur = 0
            let mutable firstI = true
            for i = 0 to statesLim do
                let mutable firstJ = true
                let good = new ResizeArray<_>(symbolsLim)
                for j = 0 to symbolsLim do
                     if checker gotos.[i,j] then
                        good.Add j
                if good.Count > 0 then
                    if not firstI then print sep
                    else firstI <- false

                    print "%d" <| pack i good.Count
                    for v = 0 to good.Count - 1 do
                        let j = good.[v]
                        print sep
                        print "%d" <| pack j gotoLists.[gotos.[i,j]]
//                        print sep
//                        let dontStackSet, stackSet = stackSets.[i,j].[0]
//                        print "%d" <| pack stackSetLists.[dontStackSet] stackSetLists.[stackSet]
                        cur <- cur + 1
                        if cur > next then
                            next <- next + 1000
                            printBr ""
                            printInd 10 ""
            printBr rBr

            resultGotoArrayCreationCodePrinter()
        
        let print2DReduceArrList (arr : 'a list[,]) checker printer name =
            print2DArrList "[|" "|]" "; " "let private" printArr resultReducesArrayCreationCodePrinter (arr : 'a list[,]) checker printer name



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
        printBr "let genLiteral (str : string) (data : %s) =" literalType
        if caseSensitive then "str"
        else "str.ToLower()"
        |> printBrInd 1 "match %s with"
            
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| \"%s\" -> Some (L_%s data)" (escapeQuotes <| indexator.indexToLiteral i) (indexator.getLiteralName i)
        printBrInd 1 "| x -> None"
        //

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

        printBrInd 0 "let isLiteral = function"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "| %s _ -> false" <| indexator.indexToTerm i
        for i = indexator.literalsStart to indexator.literalsEnd do
            printBrInd 1 "| L_%s _ -> true" (indexator.getLiteralName i)
        printBr ""

        printInd 0 "let getLiteralNames = ["
        for i = indexator.literalsStart to indexator.literalsEnd do
            print "\"%s\";" <| indexator.indexToLiteral i
        print "]"
        printBr ""

        printBr "let mutable private cur = 0"

        print "let leftSide = "
        printArr leftSide (print "%d")
        
        (*print "let private rulesStart = "
        printArr rulesStart (print "%d")*)

        printBr "let startRule = %d" grammar.startRule
        printBr ""

        printBr "let acceptEmptyInput = %A" grammar.canInferEpsilon.[leftSide.[grammar.startRule]]
        printBr ""

        print "let defaultAstToDot = "
        printBrInd 0 "(fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber None leftSide)"

        printBr ""

        printGotoArrList()

        print2DReduceArrList reduces
            (fun l -> not l.IsEmpty)
            (fun l -> printListAsArray l (fun x -> print "%d" x))
            "reduces"

        print2DReduceArrList zeroReduces
            (fun l -> not l.IsEmpty)
            (fun l -> printListAsArray l (fun x -> print "%d" x))
            "zeroReduces"

        print2DArrListDfa "[|" "|]" "; " "let private" printArr
            resultDfaArrayCreationCodePrinter
            dfaArrList 
            (fun l -> not l.IsEmpty)
            (fun l -> printListAsArray l (fun x -> print "%d" x))
            "dfaList"

        printArrHashSet "let private finiteStates = " finitStatesSet (fun x -> print "%d" x)
        print "let private amountOfStates = "
        printArr amountOfStates (fun x -> print "%d" x)

        printInd 0 "let private small_acc = "
        printList tables.acc (fun x -> print "%d" x)
        printBr ""
        printBrInd 0 "let private accStates = Array.zeroCreate %d" <| tables.gotos.GetLength 0
        printBrInd 0 "for i = 0 to %d do" statesLim
        printBrInd 1 "accStates.[i] <- List.exists ((=) i) small_acc"

        printBrInd 0 "let eofIndex = %d" grammar.indexator.eofIndex
    
        printBrInd 0 "let errorIndex = %d" grammar.errorIndex
        //printBrInd 0 "let errorRulesExists = %b" grammar.errorRulesExists
        
        printBrInd 0 "let private parserSource = new ParserSourceEBNF<Token> (gotos, reduces, zeroReduces, dfaList, finiteStates, amountOfStates, accStates, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex)"

        (*printBr "let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) ="
        printBrInd 1 "buildAstAbstract<Token> parserSource"
        printBr ""*)
        
        printBr "let buildAst : (seq<'TokenType> -> ParseResultEBNF<Token>) ="
        printBrInd 1 "buildAst<Token> parserSource"
        printBr ""
        res.ToString()

    (*let printTablesToScala () =    
        let printArr (arr : 'a[]) printer = printArr "Array" "(" ")" ", " (arr : 'a[]) printer
        let printListAsArray l printer = printList "Array" "(" ")" ", " l printer
        let printList l printer = printList "List" "(" ")" ", " l printer
        
        let resultArrayCreationCodePrinter (name:string) =
            let _type =
                match name.ToLowerInvariant() with
                | "gotos" -> "Array[Int]"
                | "reduces" -> "Array[Array[(Int,Int)]]"
                | "zeroreduces" -> "Array[Array[Int]]"
                | _ -> failwith "Unsupported array."
            let header = Printf.StringFormat<_,_>("val %s = new Array[" + _type + "]( %d )")
            printBrInd 0 header name statesCount 
            printBrInd 0 "for (i <- 0 to %d) " statesLim
            printBrInd 2 (Printf.StringFormat<_,_>("%s(i) = new " + _type + "( %d )")) name symbolsCount

            printBrInd 0 "cur = 0"
            printBrInd 0 "while (cur < small_%s.length) {" name
            printBrInd 1 "val i = small_%s(cur) >>> 16" name
            printBrInd 1 "val length = small_%s(cur) & %d" name andNum
            printBrInd 1 "cur = cur + 1"
            printBrInd 1 "for (k <- 0 to length-1){"
            printBrInd 2 "val j = small_%s(cur + k) >>> 16" name
            printBrInd 2 "val x = small_%s(cur + k) & %d" name andNum
            printBrInd 2 "%s(i)(j) = lists_%s(x)" name name
            printBrInd 2 "}"
            printBrInd 1 "cur = cur + length"
            printBrInd 1 "}"

        let print2DArrList (arr : 'a list[,]) checker printer name = 
            let mutable c = 1
            c <- c + 1            
            let r = print2DArrList "Array (" ")" ", " "val " printArr resultArrayCreationCodePrinter (arr : 'a list[,]) checker printer name
            r

        printBr "abstract class Token"        
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

            printBrInd 0 "case class %s%s extends Token" name
            <|  match type' with
                | None -> ""
                | Some s -> " ( v:" + s + ")"

        printBr ""
        printBr "class %s {" _class
        printBr ""
        printBr "def numToString (sNum:Int) = "
        printBrInd 1 "sNum match {"
        for i = 0 to indexator.nonTermCount - 1 do
            printBrInd 1 "case %d => \"%s\"" i (indexator.indexToNonTerm i)        

        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "case %d => \"%s\"" i (indexator.indexToTerm i)        

        printBrInd 1 "case _ => \"\""
        printBrInd 1 "}"


        printBrInd 0 "def tokenToNumber (token:Token) = "
        printBrInd 1 "token match {"
        for i = indexator.termsStart to indexator.termsEnd do
            printBrInd 1 "case %s (_) => %d" (indexator.indexToTerm i) i
        printBrInd 1 "}"
        printBr ""

        printBr "private var cur = 0"

        print "val leftSide = "
        printArr leftSide (print "%d")

        print "private val rules = "
        printArr rules (print "%d")

        print "private val rulesStart = "
        printArr rulesStart (print "%d")

        printBr "private val startRule = %d" grammar.startRule
        printBr ""

        printBr "private val acceptEmptyInput = %A" grammar.canInferEpsilon.[leftSide.[grammar.startRule]]
        printBr ""

        print2DArrList tables.gotos
            (fun x -> not x.IsEmpty)
            (fun x -> print "%d" x.[0])
            "gotos"
        print2DArrList reduces
            (fun l -> not l.IsEmpty)
            (fun l -> printListAsArray l (fun (x,y) -> print "(%d,%d)" x y))
            "reduces"
        print2DArrList zeroReduces
            (fun l -> not l.IsEmpty)
            (fun l -> printListAsArray l (fun (x,y) -> print "%d" x))
            "zeroReduces"
        printInd 0 "private val small_acc = "
        printList tables.acc (fun x -> print "%d" x)
        printBr ""
        printBrInd 0 "private val accStates = new Array[Boolean](%d)" <| tables.gotos.GetLength 0
        printBrInd 0 "for (i <- 0 to %d)" statesLim
        printBrInd 2 "accStates(i) = small_acc.exists(_==i)"

        printBrInd 0 "val eofIndex = %d" grammar.indexator.eofIndex

        printBrInd 0 "val errorIndex = %d" grammar.errorIndex
        printBrInd 0 "val errorRuleExists = %b" grammar.errorRulesExists

        printBrInd 0 "val parserSource = new ParserSource[Token] (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)"

        //printBr "def buildAst : (Seq[_<:Token] => ParseResult[Token]) ="
        //printBrInd 1 "buildAst[Token] parserSource"
        printBr ""
        res.ToString()*)

    match targetLanguage with
    | FSharp -> printTablesToFSharp ()
    | Scala  -> (*printTablesToScala ()*) "Not supported target language"