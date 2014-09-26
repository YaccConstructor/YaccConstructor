namespace GeneratorsCoreTest

open Mono.Addins
open Yard.Core

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type RNGLR() = 
    inherit Generator()
        override this.Name = "RNGLRGenerator"
        override this.Constraints = [||]
        override this.Generate definition = this.Generate (definition, "")

[<Extension>]
type CYKGenerator() = 
    inherit Generator()
        override this.Name = "CYKGenerator"
        override this.Constraints = [||]
        override this.Generate definition = this.Generate (definition, "")

[<Extension>]
type FParsecGenerator() = 
    inherit Generator()
        override this.Name = "FParsecGenerator"
        override this.Constraints = [||]
        override this.Generate definition = this.Generate (definition, "")

[<Extension>]
type FsYaccPrinter() = 
    inherit Generator()
        override this.Name = "FsYaccPrinter"
        override this.Constraints = [||]
        override this.Generate definition = this.Generate (definition, "")

[<Extension>]
type TreeDump() = 
    inherit Generator()
        override this.Name = "TreeDump"
        override this.Constraints = [||]
        override this.Generate definition = this.Generate (definition, "")

[<Extension>]
type YardPrinter() = 
    inherit Generator()
        override this.Name = "YardPrinter"
        override this.Constraints = [||]
        override this.Generate definition = this.Generate (definition, "")