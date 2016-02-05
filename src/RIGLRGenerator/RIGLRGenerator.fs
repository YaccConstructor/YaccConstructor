namespace Yard.Generators.RIGLRGenerator

open Mono.Addins
open Yard.Core
open Constraints

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type RIGLR() = 
    inherit Generator()
        override this.Name = "hello"
        override this.Constraints = [|noEbnf; noMeta; noInnerAlt; (*noLiterals;*) noInnerAlt; noBrackets; needAC; singleModule|]
        override this.Generate (definition, args) = box ()
        override this.Generate definition = this.Generate (definition, "")