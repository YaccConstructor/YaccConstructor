namespace Yard.Generators.GLL

open Yard.Core
open Yard.Core.IL
open Yard.Core.Checkers
open Constraints
open Yard.Generators.Common
open InitialConvert
open Yard.Generators.Common.FSA
open Yard.Generators.GLL
open Printer
open Yard.Generators.Common.FSA.Common

open AbstractAnalysis.Common
open System.Collections.Generic
open FSharp.PowerPack

type GLL() = 
    inherit Generator()
        member this.GenerateByRules (*rules*) calls locks asserts =
            let start = System.DateTime.Now
            let fsa = new FSA((*rules,*) calls, locks, asserts)
            let dfaToFsLexNfa (dfa: FSA) = 
                let nfa = new FsLex.AST.NfaNodeMap()
                let count = dfa.States.Length
                
                let counter = ref 0u
                let symbols = Dictionary<EdgeSymbol, uint32>()
                let getOrCreate symbol = 
                    if (symbols.ContainsKey symbol) then
                        symbols.Item symbol
                    else 
                        let id = !counter
                        symbols.Add(symbol, id)
                        counter := !counter + 1u
                        id
                
                [|0..count - 1|] |> Array.iter (fun i -> 
                    let node: FsLex.AST.NfaNode = 
                        {Id = i; 
                         Name = ""; 
                         Transitions = new Dictionary<_, _>(); 
                         Accepted = if (dfa.FinalStates.Contains (i * 1<positionInGrammar>)) then [i, 0] else []} 
                    nfa.GetMap.Add(i, node))
                    
                Array.iteri (fun i -> 
                    Array.iter (fun (symbol, trans) -> 
                        (nfa.GetMap.Item i).Transitions.Add(getOrCreate symbol, [nfa.Item (int trans)])))
                    dfa.States
                
                nfa, symbols
                
            let fsLexNfa, symbols = dfaToFsLexNfa fsa 
            
            printfn "fslex nfa to dfa start"
            let fsLexStart = System.DateTime.Now
            FsLex.AST.NfaToDfa fsLexNfa (fsLexNfa.GetMap.Item 0)
            FsLex.AST.NfaToDfa fsLexNfa (fsLexNfa.GetMap.Item 1)
            FsLex.AST.NfaToDfa fsLexNfa (fsLexNfa.GetMap.Item 3)
            FsLex.AST.NfaToDfa fsLexNfa (fsLexNfa.GetMap.Item 4)
            let fsLexDfa = FsLex.AST.NfaToDfa fsLexNfa (fsLexNfa.GetMap.Item 2)
            printfn "fslex nfa to dfa %A" (System.DateTime.Now - fsLexStart)
            
            printfn "yc nfa to dfa start"
            let ycStart = System.DateTime.Now
            let ycDfa = toDFA fsa.InternalFSA
            printfn "yc nfa to dfa %A" (System.DateTime.Now - ycStart)
            
            let symbols' = new Dictionary<uint32, EdgeSymbol>()
            symbols.Keys |> Seq.iter (fun k -> symbols'.Add(symbols.Item k, k))
            
            let states = 
                (snd fsLexDfa) |> List.collect (fun n -> 
                    n.Transitions |> List.map (fun (symbol, dir) -> 
                        (n.Id, (symbols'.Item symbol).ToString(), dir.Id)))
                        
            let printDotFromTransitions filePrintPath transitions = 
                let strs = new ResizeArray<_>(["digraph G {\nnode [shape = circle]"])
                
                transitions
                |> List.groupBy(fun (from, tag, target) -> from)
                |> List.iter(fun (x,_) -> sprintf "%i[label=\"%i\", style=filled]" x x |> strs.Add)
                
                transitions
                |> List.iter (fun (from, tag, target) ->
                    sprintf "%i -> %i [label=\"%s\"]; \n" from target tag |> strs.Add
                    )
                strs.Add "}"
                System.IO.File.WriteAllLines(filePrintPath, strs)
            
            printDotFromTransitions "test.dot" states
             
            //let final = (snd fsLexDfa) |> List.filter (fun n -> not n.Accepted.IsEmpty)
                        
            let generatedCode, parserSource = getGLLparserSource fsa "" (*new Map<_,_>("",Some "")*) "" false false
            
            eprintfn "Generation time: %A" <| System.DateTime.Now - start
            parserSource
            
        override this.Name = "GLLGenerator"
        override this.Constraints = [|noMeta; singleModule|]
        
        override this.Generate (definition, generateToFile, args) =
            let start = System.DateTime.Now
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length - 1 do
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
            //let mutable fullPath = getBoolOption "fullpath" false
            let mutable positionType = getOption "pos" "" id
            //let mutable needTranslate = getBoolOption "translate" false
            let mutable light = getBoolOption "light" true
            //let mutable printInfiniteEpsilonPath = getOption "infEpsPath" "" id
            //let mutable isAbstract = getBoolOption "abstract" true
            //let mutable withoutTree = ref <| getBoolOption "withoutTree" true
            let mutable outFileName =
                let fstVal = getOption "output" (definition.info.fileName + ".fs") id
                getOption "o" fstVal id

            let getBoolValue name = function
                    | "true" -> true
                    | "false" -> false
                    | value -> failwithf "Unexpected %s value %s" name value

            for opt, value in pairs do
                match opt with
                | "-module" -> moduleName <- value
                | "-token" -> tokenType <- mapFromType value
                | "-pos" -> positionType <- value
                | "-o" -> if value.Trim() <> "" then outFileName <- value
                | "-output" -> if value.Trim() <> "" then outFileName <- value
                //| "-fullpath" -> fullPath <- getBoolValue "fullPath" value
                //| "-translate" -> needTranslate <- getBoolValue "translate" value
                | "-light" -> light <- getBoolValue "light" value
                //| "-infEpsPath" -> printInfiniteEpsilonPath <- value
                //| "-abstract" -> isAbstract <- getBoolValue "abstract" value
                //| "-withoutTree" -> withoutTree := getBoolValue "withoutTree" value
                | value -> failwithf "Unexpected %s option" value
                 
            let fsa = new FSA((*definition.grammar.[0].rules,*) 1, 2, 3)
            
            
            let generatedCode, parserSource = getGLLparserSource fsa outFileName (*tokenType*) moduleName light generateToFile//isAbstract
            
            if generateToFile
            then
                use out = new System.IO.StreamWriter (outFileName)
                // TODO: write foot of definition
                out.WriteLine (generatedCode.ToString().Replace("\r\n", "\n").Replace("\n", "\r\n"))
                out.Flush()
                out.Close()
            eprintfn "Generation time: %A" <| System.DateTime.Now - start
            
            box parserSource
        override this.Generate(definition, generateTofile) = this.Generate (definition, generateTofile, "")
