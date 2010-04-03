namespace Yard.Core

open Yard.Core.IL

type IGenerator = interface
    abstract Name : string
    abstract Generate : Definition.t<Source.t,Source.t> -> string
end