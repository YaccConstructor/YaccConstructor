module Yard.Core.Manager

type IComponent = interface
    abstract Name : string
end

type Manager<'T when 'T :> IComponent> () = 
    let collection = 
        new ResizeArray<'T>(ComponentsLoader.LoadComponents(typeof<'T>) |> Seq.cast)

    let available = ResizeArray.map (fun x -> (x:>IComponent).Name) collection
    let _component name = 
        collection |> ResizeArray.tryFind (function some_function -> some_function.Name = name) 
    member public self.Available = available
    member public self.Component name = _component name  