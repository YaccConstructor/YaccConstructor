// GLLGenerator.fs contains implementation of interface Generator

namespace Yard.Generators.GLL

open Yard.Core
open IL.Production
open Constraints
open Yard.Generators.RNGLR.InitialConvert
open Yard.Generators.RNGLR.FinalGrammar
open CodeEmitter

type GLLGenerator() =
    inherit Generator()
        override this.Name = "GLLGenerator"
        override this.Constraints = [|noEbnf; noMeta; noInnerAlt; noLiterals; noBrackets; needAC; singleModule|]

        override this.Generate (definition, args) =
            // parse arguments
            let args = args.Split([|' ';'\t';'\n';'\r'|]) |> Array.filter ((<>) "")
            let pairs = Array.zeroCreate <| args.Length / 2
            for i = 0 to pairs.Length-1 do
                pairs.[i] <- args.[i * 2], args.[i * 2 + 1]
            let mutable moduleName = "Parser"
            let mutable tokenType = None
            let mutable outFileName = definition.info.fileName + ".fs"
            for opt, value in pairs do
                match opt with
                | "-module" -> moduleName <- value
                | "-token" -> tokenType <- Some value
                | "-o" -> if value.Trim() <> "" then outFileName <- value
                | _ -> failwithf "Unknown option %A" opt

            let definition =
                ConversionsManager.ConversionsManager().ApplyConversion "EliminateLeftRecursion" definition
            let newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar.[0].rules)
                        
            let out = new System.Text.StringBuilder()

            emitDebugInfo grammar out
            emitNameAndUsages moduleName out
            emitSource definition.head out
            emitTokenType tokenType grammar.indexator out
            emitGrammar grammar out
            emitSource definition.foot out

            System.IO.File.WriteAllText(outFileName, out.ToString())
            box ()

        override this.Generate definition = this.Generate (definition, "")
