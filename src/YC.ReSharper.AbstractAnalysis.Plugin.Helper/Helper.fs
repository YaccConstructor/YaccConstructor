module YC.AbstractAnalysis.Helper

open YC.ReSharper.AbstractAnalysis.Plugin.Core
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections


[<Class>]
type ReSharperHelper() =
    let processor = new LanguagesProcessor()
    member this.XmlPath l = processor.XmlPath l
    member this.ParsingFinished = processor.ParsingFinished
    member this.GetNextTree l i = processor.GetNextTree l i
    member this.LexingFinished = processor.LexingFinished
    member this.GetForestWithToken l rng = processor.GetForestWithToken l rng
    member this.Process(file) =
        let defLang (n:ITreeNode) =
            match n with 
            | :? IInvocationExpression as m ->
                match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
                | "executeimmediate" -> "TSQL"
                | "eval" -> "Calc"
                | "objnotation" -> "JSON"
                | _ -> failwith "Unsupported language for AA!"
            | _ -> failwith "Unexpected information type for language specification!" 

        let graphs = (new Approximator(file)).Approximate defLang
        let lexerErrors = new ResizeArray<_>()
        let parserErrors = new ResizeArray<_>()
        processor.Process graphs
        |> ResizeArray.iter(fun (x,y) -> lexerErrors.AddRange x; parserErrors.AddRange y)
        
        lexerErrors,parserErrors
        