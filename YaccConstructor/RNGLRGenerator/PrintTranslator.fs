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
    let tokenCall name = "_rnglr_translate_token_" + name
    let ruleName name = "_rnglr_rule_" + name
    let epsilonName name = "_rnglr_epsilon_" + name
    let childrenName = "_rnglr_children"
    let indexName = "_rnglr_index"
    let tokensSeqName = "_rnglr_tokens"

    let printArgsDeclare args other t = 
        args
        |> List.map (fun arg -> sprintf "fun %s ->" (Source.toString arg))
        |> String.concat " "
        |> (fun x -> wordL <| (x + " " + sprintf "fun (%s : %s) ->" other t))

    let printArgsCallList args other = 
        args
        |> List.map Source.toString
        |> String.concat " "
        |> (fun x -> x + " " + other)

    let printArgsCallOpt args other =
        match args with
        | None -> other
        | Some str -> Source.toString str + " " + other

//    for i = 0 to nonTermLim do
//        printfn "%d: %s" i (indexator.indexToNonTerm i)
    // Delare non-term arrays
    let count = Array.zeroCreate indexator.nonTermCount
    let index = Array.zeroCreate rules.rulesCount
    let args = Array.zeroCreate indexator.nonTermCount
    args.[indexator.nonTermToIndex "error"] <- []
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

    let defineIndex = 
        sprintf "let %s = %s" indexName (printArr index (fun x -> x.ToString()))
        |> wordL

    let declareNonTermsArrays =
        [for i = 0 to indexator.nonTermCount - 1 do
            yield wordL <| sprintf "let %s = Array.zeroCreate %d" (ruleName <| indexator.indexToNonTerm i) count.[i]]
        |> aboveListL

    // Define epsilon-trees
    let printNodeType = function
        | EpsTree -> "EpsTree"
        | NonTerm -> "NonTerm"
        | Term -> "Term"

    let rec printOnScreenMAst ind (mast : MultiAST) =
        printf "%s" <| String.replicate ind " "
        printfn "["
        mast.Value
        |> List.iter
            (fun ast ->
                printf "%s" <| String.replicate ind " "
                printfn "{nodeType = %s; number = %d; children = [|" (printNodeType ast.nodeType) ast.number
                ast.children
                |> Array.iter (printOnScreenMAst (ind+4))
                printf "%s" <| String.replicate ind " "
                printfn "|]}")
        printf "%s" <| String.replicate ind " "
        printfn "]"

    let rec printMultiAst (mast : MultiAST) =
        //printOnScreenMAst 0 mast
        mast.Value
        |> List.map
            (fun ast ->
                let printChildren =
                    ast.children
                    |> Array.map printMultiAst
                    |> String.concat ";"
                    |> (fun x -> "[|" + x + "|]")
                sprintf "{nodeType = %s; number = %d; children = %s}"
                    (printNodeType ast.nodeType) ast.number printChildren)
        |> String.concat ";"
        |> (fun x -> "ref [" + x + "]")

    let defineEpsilonTrees = 
        [for i = 0 to nonTermLim do
            if grammar.canInferEpsilon.[i] then
                yield wordL <| sprintf "let %s = %s" (epsilonName <| indexator.indexToNonTerm i) (printMultiAst grammar.epsilonTrees.[i])]
        |> aboveListL

    // Define nonTerms-translation
    let defineNonTermsTranslation = 
        [for i = 0 to nonTermLim do
            let name = indexator.indexToNonTerm i
            let body =
                let epsLayout =
                    if grammar.canInferEpsilon.[i] then
                        printArgsCallList args.[i] (sprintf "%s" <| epsilonName name)
                        |> sprintf "| EpsTree -> %s %s" (tokenCall name)
                    else
                        sprintf "| EpsTree -> failwith \"Nonterm %s can't infer epsilon\"" (indexator.indexToNonTerm i)
                let termLayout = 
                    sprintf "| Term -> failwithf \"Expected nonTerm %s expansion, but token %%A found\" %s" name (sprintf "%s.[%s.number]" tokensSeqName astName)
                let nonTermLayout = 
                    printArgsCallList args.[i] (sprintf "%s.children" astName)
                    |> sprintf "| NonTerm -> %s.[%s.[%s.number]] %s" (ruleName name) indexName astName
                [ sprintf "match %s.nodeType with" astName
                ; epsLayout; termLayout; nonTermLayout]
                |> List.map wordL
                |> aboveListL
            yield
                wordL (sprintf "let rec %s = " <| tokenCall name)
                @@-- (
                    printArgsDeclare args.[i] multiAstName "MultiAST"
                    @@-- (
                        wordL (sprintf "%s.Value" multiAstName)
                        @@ wordL "|> List.map ("
                        @@-- (
                            wordL (sprintf "fun (%s : AST) -> " astName)
                            @@-- body
                        )
                        @@ wordL ")"
                        @@ wordL "|> List.concat"
                    )
                )
        ]
        |> aboveListL

    // Realise rules
    let rec getProductionLayout num = function
        | PRef (name, args) ->
            incr num
            sprintf "(%s %s)" (tokenCall <| Source.toString name) (printArgsCallOpt args <| sprintf "%s.[%d]" childrenName !num)
            |> wordL
        | PToken name -> 
            incr num
            let name = Source.toString name
            sprintf "(match %s with | %s value -> [value] | _-> failwith \"Token %s expected\") " (sprintf "%s.[%s.[%d].Value.[0].number]" tokensSeqName childrenName !num) name name
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
                        wordL ("for " + var + " in ")
                        -- prod
                        -- wordL (" do")
                    )
                |> List.rev
                |> List.fold
                    (fun acc x -> x @@-- acc)
                    (wordL "yield (" -- actionCodeLayout -- wordL ")")
                |> (fun x -> (wordL "[" @@-- x) @@ wordL "]")
        | x -> failwithf "unexpected %A construction" x

    let getRuleLayout (rule : Rule.t<Source.t,Source.t>) =
        printArgsDeclare rule.args childrenName "MultiAST[]"
        @@-- getProductionLayout (ref -1) rule.body
    let rules = 
        srcGrammar
        |> Array.mapi
            (fun i rule ->
                (wordL <| sprintf "%s.[%d] <- (" (ruleName <| indexator.indexToNonTerm (rules.leftSide i)) index.[i])
                @@-- (getRuleLayout rule)
                @@ wordL ")")
        |> List.ofArray
        |> aboveListL
        (* tail *)
        //rules
    let funHead =
        wordL "let translate (_rnglr_tokens_init : seq<Token>) = "
        @@-- wordL "let _rnglr_tokens = Array.ofSeq _rnglr_tokens_init"
    let funRes =
        grammar.startRule
        |> grammar.rules.leftSide
        |> indexator.indexToNonTerm
        |> tokenCall
        |> sprintf "%s"
        |> wordL
    [defineIndex; defineEpsilonTrees; declareNonTermsArrays; defineNonTermsTranslation; rules; funRes]
    |> aboveListL
    |> (fun body -> funHead @@-- body)
    |> Display.layout_to_string(FormatOptions.Default)
    |> out.WriteLine
    