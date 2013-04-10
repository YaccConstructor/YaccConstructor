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
            let newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar.[0].rules)

            let mutable outFileName = definition.info.fileName + ".fs"
            let out = new System.Text.StringBuilder()            

            emitNameAndUsages "Parser" out
            emitTokenType grammar.indexator out
            emitGrammar grammar out

            System.IO.File.WriteAllText(outFileName, out.ToString())
            box ()

        override this.Generate definition = this.Generate (definition, "")
        