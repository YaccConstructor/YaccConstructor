module Yard.Core.Manager

type IComponent = interface
    abstract Name : string
end

type Manager<'T when 'T :> IComponent> () = 
    let collection: ResizeArray<'T> = 
        new ResizeArray<'T>(ComponentsLoader.LoadComponents(typeof<'T>) |> Seq.map (fun x -> x :?> 'T))

    let available = Seq.map (fun (x) -> (x:>IComponent).Name) collection
    let _component name = collection.Find (fun some_function -> some_function.Name = name) 
    member self.Available = available
    member self.Component name = _component name