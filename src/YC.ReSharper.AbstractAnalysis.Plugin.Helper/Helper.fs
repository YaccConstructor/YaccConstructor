module YC.AbstractAnalysis.Helper

open YC.ReSharper.AbstractAnalysis.Plugin.Core
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections

[<Class>]
type ReSharperHelper() =
    let processors = 
        [
            ("tsql",YC.ReSharper.AbstractAnalysis.Languages.TSQL.TSQLInjectedLangugeModule():>YC.AbstractAnalysis.CommonInterfaces.IInjectedLanguageModule<_,_,_>)
            ("calc",YC.ReSharper.AbstractAnalysis.Languages.Calc.CalcInjectedLanguageModule():>YC.AbstractAnalysis.CommonInterfaces.IInjectedLanguageModule<_,_,_>)
            ]
        |> dict    

    //let processor = new LanguagesProcessor()
    member this.XmlPath (l:string) = processors.[l.ToLowerInvariant()].XmlPath
    member this.ParsingFinished = 
        processors |> Seq.map(fun kvp -> kvp.Value.ParsingFinished) |> (fun x -> new ResizeArray<_>(x))
        //processor.ParsingFinished
    member this.GetNextTree (l:string) i = 
        processors.[l.ToLowerInvariant()].GetNextTree i
        //processor.GetNextTree l i
    member this.LexingFinished = 
        processors |> Seq.map(fun kvp -> kvp.Value.LexingFinished) |> (fun x -> new ResizeArray<_>(x))
        //processor.LexingFinished
    member this.GetForestWithToken (l:string) rng = 
        processors.[l.ToLowerInvariant()].GetForestWithToken rng
        //processor.GetForestWithToken l rng
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
//        processor.Process graphs
//        |> ResizeArray.iter(fun (x,y) -> lexerErrors.AddRange x; parserErrors.AddRange y)
        graphs
        |> ResizeArray.map (fun (l,g) -> processors.[l.ToLowerInvariant()].Process g)
        |> ResizeArray.iter(fun (x,y) -> lexerErrors.AddRange x; parserErrors.AddRange y)
        
        lexerErrors,parserErrors
