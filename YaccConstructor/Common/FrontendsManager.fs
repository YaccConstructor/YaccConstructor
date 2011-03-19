module Yard.Core.FrontendsManager

let private frontendsCollection: ResizeArray<IFrontend> = 
    new ResizeArray<IFrontend>(ComponentsLoader.LoadComponents(typeof<IFrontend>) |> Seq.map (fun x -> x :?> IFrontend))

let AvailableFrontends = Seq.map (fun (x:IFrontend) -> x.Name) frontendsCollection

let Frontend name = frontendsCollection.Find (fun frontend -> frontend.Name = name)
let Register (frontend) = frontendsCollection.Add (frontend)

let GetByExtension = function
    | "yrd" -> Some(Frontend "YardFrontend")
    | "g"   -> Some(Frontend "AntlrFrontend")
    | _     -> None

