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

namespace Yard.Generators.RNGLR

open AbstractAnalysis.Common
open System
open System.IO
open System.Text
open Yard.Core
open Yard.Core.IL
open Yard.Core.Checkers
open Constraints
open Yard.Generators.Common
open Yard.Generators.RNGLR
open InitialConvert
open Yard.Generators.Common.FinalGrammar
open States
open Printer
open TranslatorPrinter
open Option
open HighlightingPrinter
open HighlightingConvertions

type RNGLR() = 
    inherit Generator()
        override this.Name = "RNGLRGenerator"
        override this.Constraints = [|noEbnf; noMeta; noInnerAlt; (*noLiterals;*) (*noAlt;*) noBrackets; needAC; singleModule|]
        override this.Generate (definition, generateToFile, args) =
            let start = DateTime.Now
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length-1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]
            let getOption name either f =
                match definition.options.TryFind name with
                | Some v -> f v
                | None -> either
            let getBoolOption opt either =
                getOption opt either <| function
                    | "true" -> true
                    | "false" -> false
                    | x -> failwithf "Option %s expected values true or false, but %s found." opt x
            let mapFromType t = Map.ofList ["_", Some t]
            let mutable moduleName = getOption "module" "" id

            let mutable tokenType = getOption "token" definition.tokens mapFromType
            let mutable table = getOption "table" LALR <| function
                                    | "LR" -> LR
                                    | "LALR" -> LALR
                                    | x -> failwithf "Unsupported table type: %s." x
            let mutable fullPath = getBoolOption "fullpath" false
            let mutable positionType = getOption "pos" "" id
            let needTranslate = ref <| getBoolOption "translate" true
            let needHighlighting = ref <| getBoolOption "highlighting" false
            let namespaceName = ref <| getOption "namespace" "NamespaceName" id
            let mutable light = getBoolOption "light" true
            let mutable printInfiniteEpsilonPath = getOption "infEpsPath" "" id
            let mutable caseSensitive = getBoolOption "caseSensitive" false
            let mutable output =
                let fstVal = getOption "output" (definition.info.fileName + ".fs") id
                getOption "o" fstVal id
            let mutable targetLanguage =
                getOption "lang" FSharp <| function
                    | "fsharp" -> FSharp
                    | "scala" -> Scala
                    | x -> failwithf "Unsupported output language: %s." x
            let isAbstractParsingMode = ref <| getBoolOption "abstract" false
            let getBoolValue name = function
                    | "true" -> true
                    | "false" -> false
                    | value -> failwithf "Unexpected %s value %s" name value
            let mutable printPos = true

            for opt, value in pairs do
                match opt with
                | "-module" -> moduleName <- value
                | "-token" -> tokenType <- mapFromType value
                | "-pos" -> positionType <- value
                | "-o" -> if value.Trim() <> "" then output <- value
                | "-output" -> if value.Trim() <> "" then output <- value
                | "-table" ->
                    table <- 
                        match value with
                        | "LALR" -> LALR
                        | "LR" -> LR
                        | x -> failwithf "Unexpected table type %s" x
                | "-caseSensitive" -> caseSensitive <- getBoolValue "caseSensitive" value
                | "-fullpath" -> fullPath <- getBoolValue "fullPath" value
                | "-translate" -> needTranslate := getBoolValue "translate" value
                | "-highlighting" -> needHighlighting := getBoolValue "highlighting" value
                | "-namespace" -> if value.Trim() <> "" then namespaceName := value
                | "-light" -> light <- getBoolValue "light" value
                | "-infEpsPath" -> printInfiniteEpsilonPath <- value
                | "-lang" ->
                    targetLanguage <-
                        match value.ToLowerInvariant() with
                        | "fsharp" -> FSharp
                        | "scala" -> Scala
                        | s -> failwithf "Language %s is not supported" s
                | "-abstract" -> isAbstractParsingMode := getBoolValue "abstract" value
                | "-bindSrc" -> printPos <- getBoolValue "bindSrc" value
                // In other cases causes error
                | _ -> failwithf "Unknown option %A" opt
            let mutable newDefinition = initialConvert definition
            
//            if !needHighlighting 
//            then newDefinition <- highlightingConvertions newDefinition

            let grammar = new FinalGrammar(newDefinition.grammar.[0].rules, caseSensitive)

            if !needHighlighting
            then generateCsFiles grammar.indexator !namespaceName

            let printRules() =
                let printSymbol (symbol : int) =
                    if symbol < grammar.indexator.nonTermCount 
                    then grammar.indexator.indexToNonTerm symbol
                    elif symbol >= grammar.indexator.termsStart && symbol <= grammar.indexator.termsEnd 
                    then grammar.indexator.indexToTerm symbol
                    else grammar.indexator.indexToLiteral symbol
                printfn "\nrules:"
                for i = 0 to grammar.rules.rulesCount-1 do
                    printf "%4d: %s = " i <| printSymbol (grammar.rules.leftSide i)
                    for j = 0 to grammar.rules.length i - 1 do
                        printf "%s " <| printSymbol (grammar.rules.symbol i j)
                    printfn ""
            printRules ()

            if grammar.EpsilonCyclicNonTerms.Length > 0 then
                eprintfn "Grammar contains non-terminals, which can infinitely infer epsilon:"
                grammar.EpsilonCyclicNonTerms
                |> List.map (String.concat " <- ")
                |> List.iter (eprintfn "%s")
                eprintfn ""
                if printInfiniteEpsilonPath <> "" 
                then
                    Directory.CreateDirectory printInfiniteEpsilonPath |> ignore
                    for cycle in grammar.EpsilonCyclicNonTerms do
                        let nonTerm = List.head cycle
                        grammar.epsilonTrees.[grammar.indexator.nonTermToIndex nonTerm].AstToDot
                            grammar.indexator.indexToNonTerm (fun _ -> 0<token>) None grammar.rules.leftSideArr
                            (Path.Combine (printInfiniteEpsilonPath, nonTerm + ".dot"))
                grammar.epsilonTrees |> Array.iter (fun t -> if t <> null then t.EliminateCycles())
            
            let statesInterpreter = buildStates table grammar
            let tables = new Tables(grammar, statesInterpreter)
            use out = new StreamWriter(output)
            let res = new StringBuilder()
            let dummyPos = char 0
            let println (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s).Append "\n" |> ignore) x
            let print (x : 'a) =
                Printf.kprintf (fun s -> res.Append(s) |> ignore) x
            let package, _class  =
                        match moduleName with
                        | "" -> "RNGLR","Parse"
                        | s when s.Contains "." -> s.Split '.' |> Array.rev |> (fun a -> a.[0], String.concat "." a.[1..])
                        | s -> "RNGLR",s
            let printHeaders moduleName fullPath light output targetLanguage =
                let fsHeaders() = 
                    println "%s" <| getPosFromSource printPos fullPath dummyPos (defaultSource output)
                    println "module %s"
                    <|  match moduleName with
                        | "" -> "RNGLR.Parse"
                        | s -> s
                    if not light 
                    then println "#light \"off\""
                    println "#nowarn \"64\";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type"

                    if !isAbstractParsingMode
                    then 
                        println "open Yard.Generators.ARNGLR.Parser"
                        println "open AbstractAnalysis.Common"
                    else 
                        println "open Yard.Generators.RNGLR.Parser"
                    
                    println "open Yard.Generators.RNGLR"
                    println "open Yard.Generators.Common.AST"
                    println "open Yard.Generators.Common.AstNode"
                    println "open AbstractAnalysis.Common"

                    if !needHighlighting
                    then 
                        println "open YC.SDK.ReSharper.Helper"
                        println "open JetBrains.ReSharper.Psi.Tree"
                        println "open %s" !namespaceName
                        
                    match definition.head with
                    | None -> ()
                    | Some (s : Source) ->
                        println "%s" <| getPosFromSource printPos fullPath dummyPos s
                        println "%s" <| s.text + getPosFromSource printPos fullPath dummyPos (defaultSource output)

                let scalaHeaders () =

                    println "package %s" package
                    println "//import Yard.Generators.RNGLR.Parser"
                    println "//import Yard.Generators.RNGLR"
                    println "//import Yard.Generators.RNGLR.AST"

                match targetLanguage with
                | FSharp -> fsHeaders()
                | Scala -> scalaHeaders()

            printHeaders moduleName fullPath light output targetLanguage
            let tables = printTables grammar definition.head tables moduleName tokenType res targetLanguage _class positionType caseSensitive !isAbstractParsingMode !needHighlighting
            let res = 
                if not !needTranslate || targetLanguage = Scala 
                then tables
                else 
                    tables + printTranslator grammar newDefinition.grammar.[0].rules 
                                    positionType fullPath output dummyPos caseSensitive !isAbstractParsingMode !needHighlighting printPos

            let res = 
                match definition.foot with
                | None -> res
                | Some (s : Source) ->
                    res + (getPosFromSource printPos fullPath dummyPos s + "\n"
                                + s.text + getPosFromSource printPos fullPath dummyPos (defaultSource output) + "\n")
            let res =
                match targetLanguage with
                | FSharp ->
                    let init = res.Replace("\r\n", "\n")
                    let curLine = ref 1// Must be 2, but there are (maybe) some problems with F# compiler, causing to incorrect warning
                    let res = new StringBuilder(init.Length * 2)
                    for c in init do
                        match c with
                        | '\n' -> incr curLine; res.Append Environment.NewLine
                        | c when c = dummyPos -> res.Append (string !curLine)
                        | x -> res.Append x
                        |> ignore
                    res.ToString()
                | Scala -> res + "\n}"
            out.WriteLine res
            out.Close()
            eprintfn "Generation time: %A" <| DateTime.Now - start
            //(new YardPrinter()).Generate newDefinition
            box ()
        override this.Generate(definition, gtf)= this.Generate (definition, gtf, "")