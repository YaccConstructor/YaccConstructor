namespace Yard.Core

open Yard.Core.IL

type IFrontend = interface
    abstract Name : string
    abstract ParseGrammar : obj -> Definition.t<Source.t,Source.t>
    abstract ProductionTypes : string list
end
