module Addin
open Mono.Addins
open Yard.Core
open Yard.Core.IL
open Yard.Core.Helpers
open Yard.Core.Checkers
open Microsoft.FSharp.Text
open System.IO
open System.Reflection

[<assembly:AddinRoot ("YaccConstructor", "1.0")>]

AddinManager.Initialize()    
//let x = AddinManager.Registry.RegistryPath
//printfn "%A" x
//System.IO.Directory.Delete(x, true)
AddinManager.Registry.Update(null)

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

let GetFrontends() = currentFrontends.Value
let GetConversions() = currentConversions.Value
let GetGenerators() = currentGenerators.Value
let GetFrontendNames() = currentFrontendNames.Value
let GetConversionNames() = currentConversionNames.Value
let GetGeneratorNames() = currentGeneratorNames.Value

