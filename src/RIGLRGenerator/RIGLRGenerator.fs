namespace Yard.Generators.RIGLRGenerator

open Mono.Addins
open Yard.Core
open Yard.Generators.Common
open InitialConvert
open Yard.Generators.Common.FinalGrammar
open Constraints
open Automata
open System.Diagnostics

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
            let RCA = new RCA(grammar)            
            //RCA.PrintToDOT("dot.dot", (fun x -> x.ToString()))
            Set.iter (fun s -> printfn "%A" s) RCA.PopStates
            box ()
        override this.Generate definition = this.Generate (definition, "")