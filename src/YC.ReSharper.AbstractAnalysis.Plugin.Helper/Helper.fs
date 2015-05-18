module YC.SDK.ReSharper.Helper

open System.Collections.Generic
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open JetBrains.Application
open YC.ReSharper.AbstractAnalysis.LanguageApproximation.ConstantPropagation
open Microsoft.FSharp.Collections
open ReSharperExtension
open Constants
open YC.FSA.GraphBasedFsa
open YC.FSA.FsaApproximation

type br = JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression
type range = JetBrains.DocumentModel.DocumentRange
type node = JetBrains.ReSharper.Psi.Tree.ITreeNode 

let getRange =  fun (x:JetBrains.ReSharper.Psi.CSharp.Tree.ICSharpLiteralExpression) -> (x:>ITreeNode).GetDocumentRange()

let addSemantic (parent : ITreeNode) (children : ITreeNode list) = 
    let mutable prev = null
    let mutable curr = null
    let ranges = new ResizeArray<JetBrains.DocumentModel.DocumentRange>()
    
    for child in children do
        prev <- curr
        curr <- child
        curr.PersistentUserData.PutData(Parent, parent)
        ranges.AddRange (curr.UserData.GetData(Ranges))
        if prev = null
        then parent.PersistentUserData.PutData(FirstChild, curr)
        else
            prev.PersistentUserData.PutData(NextSibling, curr)
            curr.PersistentUserData.PutData(PrevSibling, prev)
    parent.PersistentUserData.PutData(LastChild, curr)
    parent.UserData.PutData(Ranges, ranges)
    
    if ranges <> null && ranges.Count > 0
    then parent.UserData.PutData(Document, ranges.[0].Document)
    
    parent

let calculatePos (grToken: FSA<char*Position<#ITreeNode>>) =    
    let ranges = 
        grToken.Edges 
        |> Seq.groupBy (fun x -> match x.Tag with |Smbl y -> (snd y).back_ref |_ -> failwith "Unexpected Eps!!") //x.BackRef)
        |> Seq.map (fun (_, brs) -> brs |> Array.ofSeq)
        |> Seq.map(fun grToken ->
            try
                let pos =  grToken |> Array.map(fun i -> match i.Tag with |Smbl y -> (snd y).start_offset |_ -> failwith "Unexpected Eps!!") //i.StartPos)
                let lengthTok = pos.Length
                let beginPosTok = pos.[0] + 1
                let endPosTok = pos.[lengthTok-1] + 2
                let grTokenBackRef = match grToken.[0].Tag with |Smbl y -> (snd y).back_ref |_ -> failwith "Unexpected Eps!!" 
                let endPos = 
                    grTokenBackRef.GetDocumentRange().TextRange.EndOffset - endPosTok 
                    - grTokenBackRef.GetDocumentRange().TextRange.StartOffset 
                grTokenBackRef.GetDocumentRange().ExtendLeft(-beginPosTok).ExtendRight(-endPos)
            with
            | e -> 
                let grTokenBackRef = match grToken.[0].Tag with |Smbl y -> (snd y).back_ref |_ -> failwith "Unexpected Eps!!"
                grTokenBackRef.GetDocumentRange())
    ranges

type Error(errors : ResizeArray<string * 'range>) =
    member this.Info = errors

[<Class>]
type ProcessErrors(lexerErrors : Error, parserErrors : Error, semanticErrors : Error) = 
    member this.LexerErrors = lexerErrors
    member this.ParserErrors = parserErrors
    member this.SemanticErrors = semanticErrors

[<Class>]
type ReSharperHelper<'range, 'node> private() =
    let getAllProcessors() =
        Shell.Instance.GetComponents<IReSharperLanguage>()

    let getProcessor (lang: string) = 
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
        let semanticErrors = new ResizeArray<_>()

        graphs
        |> ResizeArray.map (fun (lang, graph) -> (getProcessor lang).Process graph)
        |> ResizeArray.iter(fun (x, y, z) -> 
                                lexerErrors.AddRange x
                                parserErrors.AddRange y
                                semanticErrors.AddRange z
                            )
                
        new ProcessErrors(new Error(lexerErrors), new Error(parserErrors), new Error(semanticErrors))

[<Class>]
type YcHelper private() = 
    static let allYcToInt = new Dictionary<string, Dictionary<string, int>>()

    static member AddYcItem (key : string) number (lang : string)= 
        let lang = lang.ToLowerInvariant()
        let key = key.ToLowerInvariant()
        
        let dict = 
            if allYcToInt.ContainsKey lang
            then 
                allYcToInt.[lang]
            else
                let newDict = new Dictionary<string, int>()
                allYcToInt.Add(lang, newDict)
                newDict

        if not <| dict.ContainsKey key
        then dict.Add(key, number)

    static member GetNumber (lang : string) (key : string) = 
        let lang = lang.ToLowerInvariant()
        let key = key.ToLowerInvariant()

        if allYcToInt.ContainsKey lang
        then 
            let dict = allYcToInt.[lang]
            if dict.ContainsKey key 
            then dict.[key]
            else -1
        else 
            -1
