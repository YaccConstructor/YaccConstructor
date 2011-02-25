module Yard.Core.GeneratorsManager

let private generatorsCollection: ResizeArray<IGenerator> = 
    new ResizeArray<IGenerator>(ComponentsLoader.LoadComponents(typeof<IGenerator>) |> Seq.map (fun x -> x :?> IGenerator))

let AvailableGenerators = Seq.map (fun (x:IGenerator) -> x.Name) generatorsCollection
let Generator name = generatorsCollection.Find (function gen -> gen.Name = name)
let Register (gen) = generatorsCollection.Add (gen)


