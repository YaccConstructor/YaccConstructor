namespace YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation

open Hotspot
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ApproximateCsharp

open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree

open Microsoft.FSharp.Collections

type Approximator(file:IFile) = 
    // 0 for top and 3 level down
    let recursionMaxLevel = 3

    let handleHotspots hotspots = 
        hotspots
        |> ResizeArray.map (fun (hotspot : Hotspot) -> hotspot.Language, hotspot)
        |> ResizeArray.toList

    member this.Approximate (hotspots : ResizeArray<Hotspot>)= 
        
        match file with 
        | :? ICSharpFile as csFile -> 
            ApproximateFile csFile recursionMaxLevel <| handleHotspots hotspots
        | _ -> new ResizeArray<_>()//failwithf "Sorry, this file type isn't supported now"
        
    