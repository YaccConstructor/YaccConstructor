module YC.AbstractAnalysis.Helper

open YC.ReSharper.AbstractAnalysis.Plugin.Core
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.Application
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open ReSharperExtension

[<Class>]
type ReSharperHelper<'range, 'node> private() =
    let getAllProcessors() =
        Shell.Instance.GetComponents<IReSharperLanguage>()

    let getProcessor (lang  : string) = 
        let processors = getAllProcessors() |> Array.ofSeq
        let l = lang.ToLowerInvariant()
        processors
        |> Array.find (fun processor -> processor.Name.ToLowerInvariant() = l)
    
    static let instance = new ReSharperHelper<'range, 'node>()
    static member Instance = instance

    member this.XmlPath lang = (getProcessor lang).XmlPath
    member this.ParsingFinished = getAllProcessors() |> Seq.map (fun pr -> pr.ParsingFinished) |> (fun x -> new ResizeArray<_>(x))
    member this.GetNextTree lang i = (getProcessor lang).GetNextTree i
    member this.LexingFinished = getAllProcessors() |> Seq.map(fun pr -> pr.LexingFinished) |> (fun x -> new ResizeArray<_>(x))
    
    member this.GetForestWithToken lang range = (getProcessor lang).GetForestWithToken range
    member this.GetPairedRanges lang left right range toRight = (getProcessor lang).GetPairedRanges left right range toRight
    
    member this.Process(file) =
        let graphs = (new Approximator(file)).Approximate()
        let lexerErrors = new ResizeArray<_>()
        let parserErrors = new ResizeArray<_>()
        graphs
        |> ResizeArray.map (fun (lang, graph) -> (getProcessor lang).Process graph)
        |> ResizeArray.iter(fun (x, y) -> lexerErrors.AddRange x; parserErrors.AddRange y)
        
        lexerErrors, parserErrors