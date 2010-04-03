module Yard.Core.GeneratorsManager

let private generatorsCollection: ResizeArray<IGenerator> = new ResizeArray<IGenerator>()

let Generator name = generatorsCollection.Find (function gen -> gen.Name = name)
let Register (gen) = generatorsCollection.Add (gen)


