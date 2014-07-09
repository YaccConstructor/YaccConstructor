module YC.ReSharper.AbstractAnalysis.Plugin.Core

open JetBrains.Application.Progress
open JetBrains.ProjectModel
open JetBrains.ReSharper.Psi.CSharp
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Files
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open YC.ReSharper.AbstractAnalysis.Languages
open Yard.Examples.MSParser
open AbstractAnalysis.Common
open QuickGraph
open QuickGraph.Algorithms

open System
open Mono.Addins

AddinManager.Initialize()
AddinManager.Registry.Update(null)

let injectedLanguages = 
    AddinManager.GetExtensionObjects (typeof<IInjectedLanguageProcessor>)
    |> Seq.cast<IInjectedLanguageProcessor>
    |> Seq.map (fun x -> x.Name,x)
    |> dict

//let AddinJSONNames = Seq.map (fun (elem : IInjectedLanguageProcessor) -> elem.Name) AddinJSON

type LexingFinishedArgs (tokens : ResizeArray<ITreeNode>) =
     inherit System.EventArgs()

     member this.Tokens = tokens
//     member this.Lang = lang

type ParsingFinishedArgs() = 
    inherit System.EventArgs()
    member this.Lang = ""

type Processor(file) =
    
    let mutable calcForest = []
    let mutable jsonForest = []
    let mutable tsqlForest = []

    let lexingFinished = new Event<LexingFinishedArgs>()
    let parsingFinished = new Event<ParsingFinishedArgs>()

    let defLang (n:ITreeNode) =
        match n with 
        | :? IInvocationExpression as m ->
            match m.InvocationExpressionReference.GetName().ToLowerInvariant() with
            | "executeimmediate" -> injectedLanguages.["TSql"]
            | "eval" -> injectedLanguages.["Calc"]
            | "objnotation" -> injectedLanguages.["JSON"] 
            | _ -> failwith "Unsupported language for AA!"
        | _ -> failwith "Unexpected information type for language specification!"

(*    let processLang graph tokenize parse addLError addPError = 
         
    static member CalculatePos (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
    *)
(*    member this.Process () = 
        let ranges = 
            brs |> Seq.groupBy (fun x -> x.back_ref)
            |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
            |> Seq.map(fun brs ->
                try
                    let pos =  brs |> Array.map(fun i -> i.pos_cnum)
                    let lengthTok = pos.Length
                    let beginPosTok = pos.[0] + 1
                    let endPosTok = pos.[lengthTok-1] + 2 
                    let endPos = 
                        brs.[0].back_ref.GetDocumentRange().TextRange.EndOffset - endPosTok 
                        - brs.[0].back_ref.GetDocumentRange().TextRange.StartOffset 
                    brs.[0].back_ref.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
                with
                | e -> 
                    brs.[0].back_ref.GetDocumentRange())
        ranges    

    static member TokenToPos tokenData token = 
        let data = unbox <| tokenData token
        let str : string = fst data
        let pos : array<AbstractLexer.Core.Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>> 
                = snd data
                
        Processor.CalculatePos pos

    static member TranslateToTreeNode translate nextTree errors = (Seq.head <| translate nextTree errors) :> ITreeNode

    member this.Graphs() =  (new Approximator(file)).Approximate defLang
    
    member this.Process () = 
        let parserErrors = new ResizeArray<_>()
        let lexerErrors = new ResizeArray<_>()
        let filterBrs (brs:array<AbstractLexer.Core.Position<#ITreeNode>>) =
            let res = new ResizeArray<AbstractLexer.Core.Position<#ITreeNode>>(3)
            brs |> Array.iter(fun br -> if res.Exists(fun x -> obj.ReferenceEquals(x.back_ref,br.back_ref)) |> not then res.Add br)
            res.ToArray()
        
        let graphs = this.Graphs()
        let addError tok tokenToNumberFunc numToStringFunc (tokenDataFunc: _ -> obj) =
            let e t l (brs:array<AbstractLexer.Core.Position<JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression>>) = 
                Processor.CalculatePos brs 
                |> Seq.iter
                    (fun dr -> parserErrors.Add <| ((sprintf "%A(%A)" t l), dr))
            let name = tok |> (tokenToNumberFunc >>  numToStringFunc)
            let (l:string),br = tokenDataFunc tok :?> _
            e name l br
*)
        
        //let error (l:IInjectedLanguageProcessor) tok  = addError tok l.TokenToNumber l.NumToString l.TokenData 
        //let errorJSON tok  = addError tok JSON.Parser.tokenToNumber JSON.Parser.numToString JSON.Parser.tokenData

        let tokenize = fun _ -> ()
        let error tok = fun tok -> ()
        let addSppf pair = fun pair -> ()
        let func = fun _ -> ()
//        let addCalcSPPF pair = calcForest <- calcForest @ [pair]
//        let addJsonSPPF pair = jsonForest <- jsonForest @ [pair]
//        let addTSqlSPPF pair = tsqlForest <- tsqlForest @ [pair]

        |> ResizeArray.iter 
            (fun (lang, graph) ->
                processLang graph func func func func func func func 
//                match lang with
//                | Calc -> 
//                    processLang graph Calc.tokenize Calc.parse lexerErrors.Add  errorCalc Calc.translate addCalcSPPF Calc.tokenToTreeNode lang
//                | JSON -> 
//                    processLang graph JSON.tokenize JSON.parse lexerErrors.Add  errorJSON JSON.translate addJsonSPPF JSON.tokenToTreeNode lang
//                | TSQL -> 
//                    processLang graph TSQL.tokenize TSQL.parse lexerErrors.Add  errorTSQL TSQL.translate addTSqlSPPF TSQL.tokenToTreeNode lang
            )
        lexerErrors, parserErrors

    [<CLIEvent>]
    member this.LexingFinished = lexingFinished.Publish

    [<CLIEvent>]
    member this.ParsingFinished = parsingFinished.Publish