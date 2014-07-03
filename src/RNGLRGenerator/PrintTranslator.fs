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
open Yard.Generators.RNGLR.FinalGrammar
open System.Collections.Generic
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Core.IL
open Yard.Core.IL.Production
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open PrintTreeNode

let getPosFromSource fullPath dummyPos (src : Source.t) =
    let file =
        if fullPath then src.file
        else
            let start = src.file.LastIndexOfAny [|'\\'; '/'|] + 1
            src.file.Substring start
    if file = "" then
        printfn "Source without filename: %s" <| src.ToString()
        "\n"
    elif src.startPos.line = -1 then sprintf "\n# %c \"%s\"" dummyPos file
    else sprintf "\n# %d \"%s\"" src.startPos.line file

let defaultSource output = new Source.t("", new Source.Position(0,-1,0), new Source.Position(), output)

let printTranslator (grammar : FinalGrammar) (srcGrammar : Rule.t<Source.t,Source.t> list)
        positionType fullPath output dummyPos caseSensitive (highlightingOpt : string option)=
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
        |> List.map (fun arg -> sprintf "fun %s ->" (Source.toString arg))
        |> String.concat " "
        |> fun s -> s.Trim()
        |> wordL

    let printArgsCallList args other = 
        args
        |> List.map Source.toString
        |> String.concat " "
        |> (fun x -> other + " " + x)

    let printArgsCallOpt args =
        match args with
        | None -> ""
        | Some str -> Source.toString str

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
        let rec printAst : (obj -> _) =
            function
            | :? AST as arr ->
                "box (new AST(" + printChild arr.first
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
            let name = Source.toString name
            let value = 
                if name <> "error" || highlightingOpt.IsSome
                then sprintf "((unbox %s.[%d]) : '_rnglr_type_%s) " childrenName !num name
                else sprintf "((unbox %s.[%d]) : list<ErrorNode>)" childrenName !num
            value + (printArgsCallOpt args)
            |> wordL
        | PToken name -> 
            incr num
            let name = Source.toString name
            sprintf "(match ((unbox %s.[%d]) : Token) with %s _rnglr_val -> [_rnglr_val] | a -> failwith \"%s expected, but %%A found\" a )"
                childrenName !num name name
            |> wordL
        | PLiteral name -> 
            incr num
            let name = Source.toString name
            let i = Indexator.transformLiteral caseSensitive name |> indexator.literalToIndex
            sprintf "(match ((unbox %s.[%d]) : Token) with L_%s _rnglr_val -> [_rnglr_val] | a -> failwith \"%s expected, but %%A found\" a )"
                childrenName !num (indexator.getLiteralName i) name
            |> wordL
        | PSeq (s, ac, _) ->
            let getVarName innerNum (e : elem<_,_>) =
                match e.binding with
                | None ->
                    incr innerNum
                    sprintf "_rnglr_var_%d" <| !innerNum
                | Some b -> Source.toString b
                
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
                    let strings = (Source.toString ac).Replace("\r\n", "\n").Split([|'\n'|])
                    strings.[0] <- String.replicate ac.startPos.column " " + strings.[0]
                    strings
                    |> List.ofArray
                    |> (fun l -> getPosFromSource fullPath dummyPos ac ::l)
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
                        let res = prod -- wordL ("|> List.iter (fun (" + var + ") -> " + getPosFromSource fullPath dummyPos ch)
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
    let getRuleLayout (rule : Rule.t<Source.t,Source.t>) nonTermName =
        if positionType = "" then
            failwith "RNGLR: Unspecified position type"
        wordL (sprintf "fun (%s : array<_>) (parserRange : (%s * %s)) -> " childrenName positionType positionType)
        @@-- (wordL "box ("
              @@-- (wordL "(" ++ printArgsDeclare rule.args
                    @@-- getProductionLayout (ref -1) rule.body
                    @@-- wordL (")" + getPosFromSource fullPath dummyPos rule.name)
                    @@-- wordL (" : '_rnglr_type_" + nonTermName + ")")
                    -- wordL (getPosFromSource fullPath dummyPos (defaultSource output))
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
        let errorRule : Rule.t<_,_> = 
            {
                name    = new Source.t("error")
                args    = []
                body    = PSeq([], Some <| new Source.t("parserRange"), None)
                isStart = false
                isPublic = false
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
    let mainHighlightSemantic () = 
        
        
        let printAddSemantic = 
            let res  = new System.Text.StringBuilder()

            let inline print (x : 'a) =
                Printf.kprintf (fun s -> res.Append s |> ignore) x

            let inline printBr (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

            let inline printBrInd num (x : 'a) =
                print "%s" (String.replicate (num <<< 2) " ")
                printBr x

            printBrInd 0 "let addSemantic (parent : ITreeNode) (children : ITreeNode list) = " 
            printBrInd 1 "let mutable prev = null"
            printBrInd 1 "let mutable curr = null"
            printBrInd 1 "let ranges = new ResizeArray<JetBrains.DocumentModel.DocumentRange>()"
            printBrInd 1 "for child in children do"
            printBrInd 2 "prev <- curr"
            printBrInd 2 "curr <- child"
            printBrInd 2 "curr.PersistentUserData.PutData(PropertyConstant.Parent, parent)"
            printBrInd 2 "ranges.AddRange (curr.UserData.GetData(KeyConstant.Ranges))"
            printBrInd 2 "if prev = null"
            printBrInd 2 "then parent.PersistentUserData.PutData(PropertyConstant.FirstChild, curr)"
            printBrInd 2 "else"
            printBrInd 3 "prev.PersistentUserData.PutData(PropertyConstant.NextSibling, curr)"
            printBrInd 3 "curr.PersistentUserData.PutData(PropertyConstant.PrevSibling, prev)"
            printBrInd 1 "parent.PersistentUserData.PutData(PropertyConstant.LastChild, curr)"
            printBrInd 1 "parent.UserData.PutData(KeyConstant.Ranges, ranges)"
            printBrInd 1 "parent"
            res.ToString()

        let printCalculatePos = 
            let res  = new System.Text.StringBuilder()

            let inline print (x : 'a) =
                Printf.kprintf (fun s -> res.Append s |> ignore) x

            let inline printBr (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s).Append('\n') |> ignore) x

            let inline printBrInd num (x : 'a) =
                print "%s" (String.replicate (num <<< 2) " ")
                printBr x

            printBrInd 0 "let calculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) ="
            printBrInd 1 "let ranges = "
            printBrInd 2 "brs |> Seq.groupBy (fun x -> x.back_ref)"
            printBrInd 2 "|> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)"
            printBrInd 2 "|> Seq.map(fun brs ->"
            printBrInd 3 "try"
            printBrInd 4 "let pos =  brs |> Array.map(fun i -> i.pos_cnum)"
            printBrInd 4 "let lengthTok = pos.Length"
            printBrInd 4 "let beginPosTok = pos.[0] + 1"
            printBrInd 4 "let endPosTok = pos.[lengthTok-1] + 2"
            printBrInd 4 "let endPos = "
            printBrInd 5 "brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok"
            printBrInd 5 "- brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset"
            printBrInd 4 "brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)"
            printBrInd 3 "with"
            printBrInd 3 "| e -> brs.[0].back_ref.GetDocumentRange())"
            printBrInd 1 "ranges"
            res.ToString()

        if highlightingOpt.IsSome 
        then 
            let printXmlName = sprintf "let xmlPath = \"%s.xml\" %s" highlightingOpt.Value System.Environment.NewLine
            wordL <| System.String.Concat [| printXmlName; System.Environment.NewLine; 
                                                printAddSemantic; System.Environment.NewLine; 
                                                printCalculatePos|] 
        else wordL ""
    
    [ mainHighlightSemantic(); (*nowarn; *)defineEpsilonTrees; (*declareNonTermsArrays;*)rules; funRes]
    |> aboveListL
    |> Display.layout_to_string(FormatOptions.Default)