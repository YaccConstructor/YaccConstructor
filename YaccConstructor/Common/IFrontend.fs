namespace Yard.Core

open Yard.Core.IL

type IFrontend = interface
    abstract Name : string
    abstract ParseFile : string -> Definition.t<Source.t,Source.t>
    abstract ProductionTypes : string list
end
