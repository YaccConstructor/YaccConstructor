module Addin

open Yard.Core
(*
let private createFrontendsInitialization() = 
    lazy( AddinManager.GetExtensionObjects (typeof<Frontend>) |> Seq.cast<Frontend> |> Seq.toArray )
let private createConversionsInitialization() = 
    lazy( AddinManager.GetExtensionObjects (typeof<Conversion>) |> Seq.cast<Conversion> |> Seq.toArray )
let private createGeneratorsInitialization() = 
    lazy( AddinManager.GetExtensionObjects (typeof<Generator>) |> Seq.cast<Generator> |> Seq.toArray )

let mutable private currentFrontends = createFrontendsInitialization()
let mutable private currentConversions = createConversionsInitialization()
let mutable private currentGenerators = createGeneratorsInitialization()

let private createFrontendNamesInitialization() = 
    lazy( Seq.map (fun (elem : Frontend) -> elem.Name) currentFrontends.Value |> Seq.toArray )
let private createConversionNamesInitialization() = 
    lazy( Seq.map (fun (elem : Conversion) -> elem.Name) currentConversions.Value |> Seq.toArray )
let private createGeneratorNamesInitialization() = 
    lazy( Seq.map (fun (elem : Generator) -> elem.Name) currentGenerators.Value |> Seq.toArray )

let mutable private currentFrontendNames = createFrontendNamesInitialization()
let mutable private currentConversionNames = createConversionNamesInitialization()
let mutable private currentGeneratorNames = createGeneratorNamesInitialization()

let getX (x : 'a []) = 
    if x.Length = 0
    then 
        failwith "Something wrong with Addin Manager."
    else x

let GetFrontends() = getX currentFrontends.Value
let GetConversions() = getX currentConversions.Value
let GetGenerators() = getX currentGenerators.Value
let GetFrontendNames() = getX currentFrontendNames.Value
let GetConversionNames() = getX currentConversionNames.Value
let GetGeneratorNames() = getX currentGeneratorNames.Value
*)
