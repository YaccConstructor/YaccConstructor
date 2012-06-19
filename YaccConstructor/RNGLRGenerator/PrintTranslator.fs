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

module Yard.Generators.RNGLR.TranslatorPrinter

open Yard.Generators.RNGLR.FinalGrammar
open System.Collections.Generic
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST
open Yard.Core.IL
open Yard.Core.IL.Production
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let printTranslator (grammar : FinalGrammar) (srcGrammar : Rule.t<Source.t,Source.t> list) (out : System.IO.StreamWriter) =
    let tab = 4
    let print (x : 'a) =
        fprintf out x
    let printInd num (x : 'a) =
        print "%s" (String.replicate (tab * num) " ")
        print x

    let rules = grammar.rules
    let srcGrammar = Array.ofList srcGrammar
    let indexator = grammar.indexator
    let nonTermLim = indexator.nonTermCount - 1

    let multiAstName = "_rnglr_multi_ast"
    let astName = "_rnglr_ast"
    let resCycleName = "_rnglr_cycle_res"
    let nodeName = "_rnglr_node"
    let tokenCall = "_rnglr_translate_token"
    let ruleName name = "_rnglr_rule_" + name
    let ruleCall name = "_rnglr_call_rule_" + name
    let epsilonName name = "_rnglr_epsilon_" + name
    let resultCountName name = "_rnglr_res_count_" + name
    let resultName name = "_rnglr_result_" + name
    //let dfsCall = "_rnglr_dfs_call"
    let childrenName = "_rnglr_children"
    let indexName = "_rnglr_index"
    let tokensSeqName = "_rnglr_tokens"
    let pathToModule = "Yard.Generators.RNGLR.AST."
    let stackCallsIndName = "_rnglr_stack_calls_ind"
    let stackCallsNodeName = "_rnglr_stack_calls_node"
    let stackResName = "_rnglr_stack_res"

    let printArgsDeclare args= 
        args
        |> List.map (fun arg -> sprintf "fun %s ->" (Source.toString arg))
        |> String.concat " "
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

    // Delare non-term arrays
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

    let printArr (arr : 'a[]) (printer: 'a -> string) =
        let res = new System.Text.StringBuilder()
        let append (s : string) = res.Append s |> ignore
        append "[|"
        for i = 0 to arr.Length-1 do
            if i <> 0 then append "; "
            append (printer arr.[i])
        append "|]"
        res.ToString()

    let aboveStringListL = List.map wordL >> aboveListL

    let defineIndex = 
        sprintf "let %s = %s" indexName (printArr index (fun x -> x.ToString()))
        |> wordL

    let declareStacks =
        wordL (sprintf "let %s = new System.Collections.Generic.Stack<_>()" stackCallsIndName)
        @@ wordL (sprintf "let %s = new System.Collections.Generic.Stack<_>()" stackCallsNodeName)
        //@@ wordL (sprintf "let %s = ref []" stackResName) System.Collections.Generic.Queue
        @@ wordL (sprintf "let %s = new ResizeArray<_>()" stackResName)


    let dfsCond name =
        let count = resultCountName name
        [sprintf "if !num = -1 then"
        ;sprintf "num := !%s" count
        ; sprintf "incr %s;" count
        ]
        |> List.map wordL
        |> (function (h::t) -> h @@-- (aboveListL t) | _ -> emptyL)

    let addNodeToStack node ind =
        sprintf "if !(_rnglr_treenum %s) = -1 then (" node
                    + stackCallsIndName + ".Push(" + (sprintf "%d" ind) + ");"+ stackCallsNodeName + ".Push(" + node + "))";//::!" + stackCallsName + "; "


    let declareNonTermsArrays =
        [for i = 0 to indexator.nonTermCount - 1 do
            yield wordL <| sprintf "let %s = Array.zeroCreate %d" (ruleName <| indexator.indexToNonTerm i) count.[i]
            yield wordL <| sprintf "let %s = Array.zeroCreate %d" (ruleCall <| indexator.indexToNonTerm i) count.[i]
            yield wordL <| sprintf "let %s = ref 0" (resultCountName <| indexator.indexToNonTerm i)
            yield wordL <| sprintf "let %s = ref <| Array.zeroCreate 0" (resultName <| indexator.indexToNonTerm i)
        ]
        |> aboveListL

    let printNode =
        let rec inner isFirst (node : AST<_>) =
            (getFamily node).Value
            |> List.map
                (function
                | Inner (num, children) ->
                    let printChildren =
                        children
                        |> Array.map (inner false)
                        |> String.concat ";"
                        |> (fun x -> "[|" + x + "|]")
                    sprintf "%sInner (%d, %s) " pathToModule num printChildren
                | Epsilon -> pathToModule + "Epsilon")
            |> String.concat ";"
            |> (fun x -> pathToModule + "NonTerm (ref [" + x + "], ref -1)")
        inner true

    let defineEpsilonTrees = 
        [for i = 0 to nonTermLim do
            if grammar.canInferEpsilon.[i] then
                yield wordL <| sprintf "let %s = %s |> %schooseSingleAst" (epsilonName <| indexator.indexToNonTerm i)
                                    (printNode grammar.epsilonTrees.[i]) pathToModule]
        |> aboveListL

    let defineUtils = 
        let res = 
            [sprintf "let inline _rnglr_treenum x = match x with | %sNonTerm (_,v) -> v | _ -> failwith \"NonTerminal expected, but terminal found.\" " pathToModule
            //;sprintf "let inline _rnglr_treeval = fst"
            ;sprintf "let inline _rnglr_pop (x : ResizeArray<_>) = " 
            ]
            |> aboveStringListL
        res
        @@-- (
            ["let pos = x.Count - 1"
            ;"let res = x.[pos]"
            ;"x.RemoveAt(pos)"
            ;"res"
            ]
            |> aboveStringListL
        )

    let declareNonTems =
        [sprintf "let private %s = Array.zeroCreate %d" tokenCall (indexator.nonTermCount * 2)
        //;sprintf "let %s = Array.zeroCreate %d" dfsCall indexator.nonTermCount
        ]
        |> aboveStringListL

    let defineNonTermsTranslation = 
        [for i = 0 to nonTermLim do
            let name = indexator.indexToNonTerm i
            let body =
                let epsLayout =
                    if grammar.canInferEpsilon.[i] then
                        printArgsCallList args.[i] ""
                        |> sprintf "| %sEpsilon -> %s.Value.[!(_rnglr_treenum %s)] %s" pathToModule (resultName name) (epsilonName name)
                    else
                        sprintf "| %sEpsilon -> failwith \"Nonterm %s can't infer epsilon\"" pathToModule (indexator.indexToNonTerm i)
                let nonTermLayout = 
                    printArgsCallList args.[i] "_rnglr_children"
                    |> sprintf "| %sInner (_rnglr_number, _rnglr_children) -> %s.[%s.[_rnglr_number]] %s" pathToModule (ruleName name) indexName
                [ sprintf "match %s with" astName
                ; epsLayout; nonTermLayout]
                |> aboveStringListL
            yield
                //wordL (sprintf "%s.[%d] <- " tokenCall i)
                wordL (sprintf "%s.[%d] <- fun (%s : %sAST<Token>) -> " tokenCall (i + indexator.nonTermCount) nodeName pathToModule)
                @@-- (
                    wordL (sprintf "match %s with" nodeName)
                    @@ wordL (sprintf "| %sTerm _ -> failwith \"Nonterminal %s expected, but terminal found\" " pathToModule name)
                    @@ wordL (sprintf "| %sNonTerm (%s,num) ->" pathToModule multiAstName)
                    @@-- (
                        //wordL (sprintf "printfn \"tr %s\"" name)
                        //@@
                        wordL "("
                            -- printArgsDeclare args.[i]
                        @@-- (
                            wordL (sprintf "%s.Value" multiAstName)
                            @@ wordL "|> List.map ("
                            @@-- (
                                wordL (sprintf "fun (%s : %sChild<Token>) -> " astName pathToModule)
                                @@-- body
                            )
                            @@ wordL ")"
                            @@ wordL "|> List.concat"
                        )
                        @@ wordL (sprintf ") |> (fun res -> %s.Value.[!num] <- res)" (resultName name))
                    )
                )
        ]
        |> aboveListL

    // Define nonTerms-translation
    let defineNonTermsStackCalls = 
        [for i = 0 to nonTermLim do
            let name = indexator.indexToNonTerm i
            let body =
                let epsLayout =
                    if grammar.canInferEpsilon.[i] then
                        sprintf "| %sEpsilon -> " pathToModule
                           + (addNodeToStack (epsilonName name) i)
                    else
                        sprintf "| %sEpsilon -> failwith \"Nonterm %s can't infer epsilon\"" pathToModule (indexator.indexToNonTerm i)
                let nonTermLayout = 
                    sprintf "| %sInner (_rnglr_number, _rnglr_children) -> %s.[%s.[_rnglr_number]] _rnglr_children" pathToModule (ruleCall name) indexName
                [ sprintf "match %s with" astName
                ; epsLayout; nonTermLayout]
                |> aboveStringListL
            yield
                //wordL (sprintf "%s.[%d] <- fun (%s : %sNode<Token>) -> " dfsCall i nodeName pathToModule)
                wordL (sprintf "%s.[%d] <- fun (%s : %sAST<Token>) -> " tokenCall i nodeName pathToModule)
                @@-- (
                    wordL (sprintf "match %s with" nodeName)
                    @@ wordL (sprintf "| %sTerm _ -> failwith \"Nonterminal %s expected, but terminal found\" " pathToModule name)
                    @@ wordL (sprintf "| %sNonTerm (%s,num) ->" pathToModule multiAstName)
                    @@-- (
                        dfsCond name
                        @@-- ( // Not a bug!!! At least, by design.
                            //wordL (stackCallsName + ".Push(" + (sprintf "%d" (i + indexator.nonTermCount)) + "," + nodeName + ");")//::!" + stackCallsName + "; ")
                            wordL (stackCallsIndName + ".Push(" + (sprintf "%d" (i + indexator.nonTermCount)) + ")")
                            @@ wordL (stackCallsNodeName + ".Push(" + nodeName + ")")
                            @@ wordL (sprintf "%s.Value" multiAstName)
                            @@ wordL "|> List.iter ("
                            @@-- (
                                wordL (sprintf "fun (%s : %sChild<Token>) -> " astName pathToModule)
                                @@-- body
                            )
                            @@ wordL ")"
                        )
                    )
                )
        ]
        |> aboveListL

    // Realise rules
    let rec getProductionLayout num = function
        | PRef (name, args) ->
            incr num
            let value = sprintf "%s.Value.[(_rnglr_treenum %s.[%d]).Value] " (resultName (Source.toString name)) childrenName !num
            value + (printArgsCallOpt args)
            |> wordL
        | PToken name -> 
            incr num
            let name = Source.toString name
            sprintf "(match %s with | %sTerm (%s value) -> [value] | _-> failwith \"Token %s expected\") "
                (sprintf "%s.[%d]" childrenName !num) pathToModule name name
            |> wordL
        | PSeq (s, ac) ->
            match ac with
            | None -> wordL "[]"
            | Some ac ->
                let actionCodeLayout =
                    (Source.toString ac).Split([|'\r'; '\n'|])
                    |> Array.filter ((<>) "")
                    |> List.ofArray
                    |> List.map wordL
                    |> aboveListL
                s
                |> List.map
                    (fun e ->
                        let var = 
                            if e.binding.IsNone || e.omit then (sprintf "_rnglr_var_%d" <| !num + 1)
                            else Source.toString e.binding.Value
                        let prod = getProductionLayout num e.rule
                        prod
                        -- wordL ("|> List.iter (fun (" + var + ") -> ")
                        //-- wordL (" do")
                    )
                |> List.rev
                |> List.fold
                    (fun acc x -> x @@-- acc -- wordL ")")
                    (wordL (resCycleName + " := (") -- actionCodeLayout -- wordL (")::!" + resCycleName))
                |> (fun x -> [wordL <| "let " + resCycleName + " = ref []"
                              x
                              wordL <| "!" + resCycleName
                             ] |> aboveListL)
                |> (fun x -> (wordL "(" @@-- x) @@ wordL ")")
        | x -> failwithf "unexpected %A construction" x

    let getRuleLayout (rule : Rule.t<Source.t,Source.t>) =
        wordL (sprintf "fun (%s : %sAST<Token>[]) -> " childrenName pathToModule)
        -- printArgsDeclare rule.args
        @@-- getProductionLayout (ref -1) rule.body

    let getRuleCallLayout rule =
        let head = wordL <| sprintf "fun (%s : %sAST<_>[]) ->" childrenName pathToModule
        let body =
            let prod = rules.rightSide rule
            [for sub = 0 to prod.Length - 1 do
                if prod.[sub] < indexator.nonTermCount then
                    yield addNodeToStack (sprintf "%s.[%d]" childrenName sub) prod.[sub]
             yield "()"
            ]
            |> aboveStringListL
        head @@-- body

    let aboveArrayL = List.ofArray >> aboveListL
    let callsRules = 
        srcGrammar
        |> Array.mapi
            (fun i rule ->
                wordL (sprintf "%s.[%d] <- " (ruleCall <| indexator.indexToNonTerm (rules.leftSide i)) index.[i])
                @@-- getRuleCallLayout i)
        |> aboveArrayL
    let rules = 
        srcGrammar
        |> Array.mapi
            (fun i rule ->
                wordL (sprintf "%s.[%d] <- " (ruleName <| indexator.indexToNonTerm (rules.leftSide i)) index.[i])
                @@-- getRuleLayout rule)
        |> aboveArrayL
    let funHead = wordL "let translate node = "
    let funRes =
        let startInd = 
            grammar.startRule
            |> grammar.rules.leftSide
        let start_name = 
            startInd
            |> indexator.indexToNonTerm
        let initCounts =
            [for i = 0 to indexator.nonTermCount - 1 do
                yield wordL <| sprintf "%s := 0" (resultCountName <| indexator.indexToNonTerm i)
            ]
            |> aboveListL
        let buildCallStack =
            let cycleBody =
                (*[sprintf "let n,i,x = _rnglr_pop %s" stackCallsName
                ;sprintf "if n then %s.[i] x" dfsCall
                ;sprintf "else %s.Add(i,x) |> ignore" stackResName
                ]*)
                [
                 //sprintf "let i,x = %s.Pop()" stackCallsName
                 sprintf "let i = %s.Pop()" stackCallsIndName
                 sprintf "let x = %s.Pop()" stackCallsNodeName
                 sprintf "if i < %d then %s.[i] x" indexator.nonTermCount tokenCall
                 sprintf "else %s.Add(i,x)" stackResName
                ]
                |> List.map wordL
                |> aboveListL
            [
             //sprintf "%s.Clear()" stackCallsName
             sprintf "%s.Clear()" stackCallsIndName
             sprintf "%s.Clear()" stackCallsNodeName
            //;(sprintf "%s := []" stackResName)
             sprintf "%s.Clear()" stackResName
             addNodeToStack "node" startInd
             sprintf "while %s.Count > 0 do" stackCallsIndName
            ]
            |> aboveStringListL
            |> (fun x -> x @@-- cycleBody)
        let setDimensions = 
            [for i = 0 to indexator.nonTermCount - 1 do
                let name = indexator.indexToNonTerm i
                yield wordL <| sprintf "%s := Array.zeroCreate !%s" (resultName name) (resultCountName name)
            ]
            |> aboveListL
        let calculateRes =
            //stackResName + ".Value |> List.iter (fun (f,x) -> f x)"
            "for (i,x) in " + stackResName + (sprintf " do %s.[i] x" tokenCall)
             //sprintf "%s (%sgetFamily node)" (tokenCall start_name) pathToModule
            |> wordL
        
        let res = resultName start_name +  ".Value.[!(_rnglr_treenum node)]" |> wordL
        [initCounts; buildCallStack; setDimensions; calculateRes; res]
        |> aboveListL
    [defineIndex; declareStacks; defineEpsilonTrees; declareNonTermsArrays; defineUtils;
     declareNonTems; defineNonTermsTranslation; defineNonTermsStackCalls; callsRules; rules]
    |> aboveListL
    |> (fun body -> body @@ funHead @@-- funRes)
    |> Display.layout_to_string(FormatOptions.Default)
    |> out.WriteLine
    