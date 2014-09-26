namespace FrontendsCoreTest

open Mono.Addins
open Yard.Core

[<assembly:Addin>]
[<assembly:AddinDependency ("YaccConstructor", "1.0")>]
do()

[<Extension>]
type AntlrFrontend() = 
    inherit Frontend()
        override this.Name = "AntlrFrontend"
        override this.ParseGrammar t = defaultDefinition List.empty
        override this.ProductionTypes = List.empty

[<Extension>]
type FsYaccFrontend() = 
    inherit Frontend()
        override this.Name = "FsYaccFrontend"
        override this.ParseGrammar t = defaultDefinition List.empty
        override this.ProductionTypes = List.empty

[<Extension>]
type IronyFrontend() = 
    inherit Frontend()
        override this.Name = "IronyFrontend"
        override this.ParseGrammar t = defaultDefinition List.empty
        override this.ProductionTypes = List.empty

[<Extension>]
type YardFrontend() = 
    inherit Frontend()
        override this.Name = "YardFrontend"
        override this.ParseGrammar t = defaultDefinition List.empty
        override this.ProductionTypes = List.empty