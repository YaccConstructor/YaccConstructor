// GLLGenerator.fs contains implementation of interface Generator

namespace Yard.Generators.GLL

open Yard.Core
open IL.Production
open Yard.Generators.RNGLR.InitialConvert
open Yard.Generators.RNGLR.FinalGrammar
open CodeEmitter

type GLLGenerator() =
    inherit Generator()
        override this.Name = "GLLGenerator"
        override this.Generate (definition, args) =
            let newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar)

            let mutable outFileName = definition.info.fileName + ".fs"
            let out = new System.Text.StringBuilder()            

            emitNameAndUsages "Parser" out
            emitGrammar grammar out

            System.IO.File.WriteAllText(outFileName, out.ToString())
            box ()

        override this.Generate definition = this.Generate (definition, "")
        override this.AcceptableProductionTypes =
            [ "PAlt"; "PToken"; "PLiteral" ]