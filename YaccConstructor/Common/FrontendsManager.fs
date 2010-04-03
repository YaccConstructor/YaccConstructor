module Yard.Core.FrontendsManager

let private frontendsCollection: ResizeArray<IFrontend> = new ResizeArray<IFrontend>()

let Frontend name = frontendsCollection.Find (function frontend -> frontend.Name = name)
let Register (frontend) = frontendsCollection.Add (frontend)

