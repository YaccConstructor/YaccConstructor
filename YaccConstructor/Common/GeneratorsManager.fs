module Yard.Core.GeneratorsManager

open Yard.Core.IL

type IGenerator = 
    abstract Generate:Definition.t<Source.t,Source.t> -> string

let private generatorsCollection: System.Collections.Generic.IDictionary<string, IGenerator> =  dict []

let Generator name = generatorsCollection.Item name

let Register (name,gen) = generatorsCollection.Add (name,gen)