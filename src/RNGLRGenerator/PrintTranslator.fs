//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Generators.RNGLR.TranslatorPrinter

open System
open Yard.Generators.Common.FinalGrammar
open System.Collections.Generic
open Yard.Generators.Common
open Yard.Generators.Common.AST
open Yard.Generators.Common.AstNode
open Yard.Core.IL
open YC.PrettyPrinter.Pretty
open YC.PrettyPrinter.StructuredFormat
open Yard.Generators.Common.Epsilon
open HighlightingPrinter

let getPosFromSource printPositions fullPath dummyPos (src : Source) =
    let file =
        if fullPath then src.file
        else
            let start = src.file.LastIndexOfAny [|'\\'; '/'|] + 1
            src.file.Substring start
    if printPositions
    then 
      if file = "" then
        printfn "Source without filename: %s" <| src.ToString()
        "\n"
      elif src.startPos.line = -1 then sprintf "\n# %c \"%s\"" dummyPos file
      else sprintf "\n# %d \"%s\"" src.startPos.line file
    else "\n"

let defaultSource output = new Source("", new SourcePosition(0,-1,0), new SourcePosition(), output)

let printTranslator (grammar : FinalGrammar) (srcGrammar : Rule<Source,Source> list)
        positionType fullPath output dummyPos caseSensitive isAbstractParsingMode isHighlightingMode printPositions =
    let tab = 4

    let rules = grammar.rules
    let srcGrammar = Array.ofList srcGrammar
    let indexator = grammar.indexator
    let nonTermLim = indexator.nonTermCount - 1

    let resCycleName = "_rnglr_cycle_res"
    let nodeName = "_rnglr_node"
    let tokenCall = "_rnglr_translate_token"
    let ruleName = "_rnglr_rule_"
    let epsilonName = "_rnglr_epsilons"
    let epsilonNameFiltered = "_rnglr_filtered_epsilons"
    let childrenName = "_rnglr_children"
    let concatsName = "_rnglr_concats"
    //let pathToModule = "Yard.Generators.RNGLR.AST."

    let printArgsDeclare args= 
        args
        |> List.map (fun arg -> sprintf "fun %s ->" (sourceToString arg))
        |> String.concat " "
        |> fun s -> s.Trim()
        |> wordL

    let printArgsCallList args other = 
        args
        |> List.map sourceToString
        |> String.concat " "
        |> (fun x -> other + " " + x)

    let printArgsCallOpt args =
        match args with
        | None -> ""
        | Some str -> sourceToString str

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
    
    for i = 0 to indexator.nonTermCount-1 do
        if count.[i] = 0 then
            args.[i] <- []

    let printArr (arr : 'a[]) (printer: 'a -> string) =
        if arr = null then "null"
        else
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


    let toStr (x : int) = x.ToString()
    let defineEpsilonTrees =
        let rec printAst : (AstNode -> _) =
            function
            | :? AST as arr ->
                //(if isAbstractParsingMode then "" else "box ") + 
                "(new AST(" + printChild arr.first
                        + ", " + printArr arr.other printChild + "))"
            | _ -> failwith "SingleNode was not expected in epsilon tree"
        and printChild (family : Family) = "new Family(" + toStr family.prod + ", new Nodes("
                                            + printArr (family.nodes.map id) printAst + "))"
        let printEps name = 
            "let " + name + " : Tree<Token>[] = " +
                printArr grammar.epsilonTrees
                    (function
                     | null -> "null"
                     | tree -> "new Tree<_>(null," + printAst tree.Root + ", null)")
            |> wordL
        printEps epsilonName
        @@
        printEps epsilonNameFiltered
        @@
        (wordL <| "for x in " + epsilonNameFiltered + " do if x <> null then x.ChooseSingleAst()")

    // Realise rules
    let rec getProductionLayout num = function
        | PRef (name, args) ->
            incr num
            let name = sourceToString name
            let value = 
                if name <> "error" || isHighlightingMode
                then sprintf "((unbox %s.[%d]) : '_rnglr_type_%s) " childrenName !num name
                else sprintf "((unbox %s.[%d]) : list<ErrorNode<Token>>)" childrenName !num
            value + (printArgsCallOpt args)
            |> wordL
        | PToken name -> 
            incr num
            let name = sourceToString name
            sprintf "(match ((unbox %s.[%d]) : Token) with %s _rnglr_val -> [_rnglr_val] | a -> failwithf \"%s expected, but %%A found\" a )"
                childrenName !num name name
            |> wordL
        | PLiteral name -> 
            incr num
            let name = sourceToString name
            let i = Indexator.transformLiteral caseSensitive name |> indexator.literalToIndex
            sprintf "(match ((unbox %s.[%d]) : Token) with L_%s _rnglr_val -> [_rnglr_val] | a -> failwithf \"%s expected, but %%A found\" a )"
                childrenName !num (indexator.getLiteralName i) name
            |> wordL
        | PSeq (s, ac, _) ->
            let getVarName innerNum (e : ProductionElem<_,_>) =
                match e.binding with
                | None ->
                    incr innerNum
                    sprintf "_rnglr_var_%d" <| !innerNum
                | Some b -> sourceToString b
                
            let actionCodeLayout =
                match ac with
                | None ->
                    let innerNum = ref -1
                    s
                    |> List.choose (fun e ->
                        let var = getVarName innerNum e
                        if e.omit then None
                        else Some var
                    ) |> String.concat ","
                    |> (fun s -> "(" + s + ")")
                    |> wordL
                | Some ac ->
                    let strings = (sourceToString ac).Replace("\r\n", "\n").Split([|'\n'|])
                    strings.[0] <- String.replicate ac.startPos.column " " + strings.[0]
                    strings
                    |> List.ofArray
                    |> (fun l -> getPosFromSource printPositions fullPath dummyPos ac ::l)
                    |> List.map wordL
                    |> aboveListL
            let innerNum = ref -1
            s
            |> List.collect
                (fun e ->
                    let var = getVarName innerNum e
                    let prod = getProductionLayout num e.rule
                    match e.checker with
                    | None -> [prod -- wordL ("|> List.iter (fun (" + var + ") -> ")]
                    | Some ch ->
                        let res = prod -- wordL ("|> List.iter (fun (" + var + ") -> " + getPosFromSource printPositions fullPath dummyPos ch)
                        let cond = wordL <| "if (" + ch.text + ") then (" 
                        [res; cond]
                    //-- wordL (" do")
                )
            |> List.rev
            |> List.fold
                (fun acc x -> x @@-- acc -- wordL ")")
                (wordL (resCycleName + " := (") @@-- actionCodeLayout @@-- wordL (")::!" + resCycleName))
            |> (fun x ->
                [wordL <| "let " + resCycleName + " = ref []"
                 x
                 //wordL <| getPosFromSource fullPath dummyPos ac
                 wordL <| "!" + resCycleName
                ]
                |> aboveListL)
            |> (fun x -> (wordL "(" @@-- x) @@ wordL ")")
        | x -> failwithf "unexpected construction: %A" x
    let getRuleLayout (rule : Rule<Source,Source>) nonTermName =
        if positionType = "" then
            failwith "RNGLR: Unspecified position type"
        wordL (sprintf "fun (%s : array<_>) (parserRange : (%s * %s)) -> " childrenName positionType positionType)
        @@-- (wordL "box ("
              @@-- (wordL "(" ++ printArgsDeclare rule.args
                    @@-- getProductionLayout (ref -1) rule.body
                    @@-- wordL (")" + getPosFromSource printPositions fullPath dummyPos rule.name)
                    @@-- wordL (" : '_rnglr_type_" + nonTermName + ")")
                    -- wordL (getPosFromSource printPositions fullPath dummyPos (defaultSource output))
                    //@@-- wordL ("")
                    )
             )
        |> fun res ->
            wordL "("
            @@-- res
            @@-- wordL ");"

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

    let rules =
        let allTypes =
            [for i = 0 to args.Length - 1 do
                yield "'_rnglr_type_" + indexator.indexToNonTerm i ]
            |> List.reduce (fun l r -> l + " * " + r)
        let errorRule : Rule<_,_> = 
            {
                name    = new Source("error")
                args    = []
                body    = PSeq([], Some <| new Source("parserRange"), None)
                isStart = false
                isPublic = false
                isInline = false
                metaArgs= []
            }
        wordL ("let _rnglr_extra_array, " + ruleName + ", " + concatsName + " = ")
        @@--
           (wordL ("(Array.zeroCreate 0 : array<" + allTypes + ">), ")
            @@ wordL "[|"
            @@
            (srcGrammar
             |> Array.mapi (fun i rule ->
                    let name = indexator.indexToNonTerm <| rules.leftSide i
                    getRuleLayout rule name
             ) |> aboveArrayL
             |> fun res -> res @@ getRuleLayout errorRule "error")
            @@ (wordL "|] , [|"
                @@-- concats)
            @@ wordL "|] ")

    let funRes =
        let typeName = "'_rnglr_type_" + indexator.indexToNonTerm (grammar.rules.leftSide grammar.startRule)
        let funHead = wordL ("let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : " + typeName + " = ")
        let body =
            [yield wordL ("unbox (tree.Translate " + ruleName + " " + " leftSide " + concatsName
                            + " (if args.filterEpsilons then " + epsilonNameFiltered + " else " + epsilonName + ")"
                            + " args.tokenToRange args.zeroPosition args.clearAST dict) : " + typeName)
            ] |> aboveListL
        funHead @@-- body

    //let nowarn = wordL "#nowarn \"64\";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type"
    [(*nowarn; *)defineEpsilonTrees; (*declareNonTermsArrays;*)rules; funRes]
    |> aboveListL
    |> print 80