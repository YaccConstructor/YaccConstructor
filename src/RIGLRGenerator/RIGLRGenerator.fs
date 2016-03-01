namespace Yard.Generators.RIGLRGenerator

open Mono.Addins
open Yard.Core
open Yard.Generators.Common
open InitialConvert
open Yard.Generators.Common.FinalGrammar
open Constraints
open Automata

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type RIGLR() = 
    inherit Generator()
        override this.Name = "hello"
        override this.Constraints = [|noEbnf; noMeta; noInnerAlt; (*noLiterals;*) noInnerAlt; noBrackets; needAC; singleModule|]
        override this.Generate (definition, args) = 
            let mutable newDefinition = initialConvert definition
            let grammar = new FinalGrammar(newDefinition.grammar.[0].rules, true)
            let automaton = constructRIA grammar
            let x = automaton.Edges
            printfn "%A" <| x
            box ()
        override this.Generate definition = this.Generate (definition, "")