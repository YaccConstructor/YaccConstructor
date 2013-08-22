namespace YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
open JetBrains.ReSharper.Daemon.CSharp.Stages
open JetBrains.ReSharper.Feature.Services.Bulbs
open JetBrains.ReSharper.Feature.Services.CSharp.Bulbs
open JetBrains.ReSharper.Feature.Services.LinqTools
open JetBrains.ReSharper.Intentions.Extensibility
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
//open System.

//Метод не найден: "Microsoft.FSharp.Core.FSharpFunc`2<Microsoft.FSharp.Core.FSharpFunc`2<System.Collections.Generic.IEnumerable`1<System.Tuple`2<Int32,System.Tuple`2<!0,Int32>[]>>,ParseResult`1<!0>>,Microsoft.FSharp.Core.FSharpFunc`2<AbstractParsing.Common.ParserInputGraph`1<!0>,ParseResult`1<!0>>> Parser`1.get_Parse()".
            //base {Microsoft.FSharp.Core.FSharpFunc<Microsoft.FSharp.Core.F...Generators.RNGLR.Parser.ParseResult<Calc.AbstractParser.Token>>>} = {Yard.Generators.RNGLR.AbstractParser.get_Parse@39<Calc.AbstractParser.Token>}
type Processor(provider: ICSharpContextActionDataProvider) = 
    member this.Process () = 
        let sourceFile = provider.SourceFile
        let file = provider.SourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) :?> ICSharpFile
        let graphs = (new Approximator(file)).Approximate()
        let tokenized = graphs |> ResizeArray.map YC.Resharper.AbstractAnalysis.Languages.Calc.tokenize |> Array.ofSeq
        let parser = new Yard.Generators.RNGLR.AbstractParser.Parser<_>() 
        let ttt= parser.xx()
        let t = parser.Parse_x()   
        //YC.Resharper.AbstractAnalysis.Languages.Calc.parser()
        let _do = YC.Resharper.AbstractAnalysis.Languages.Calc.parse parser
        let parserRes = tokenized |> Array.map _do |> Array.ofSeq
        tokenized,parserRes
