namespace VSYardNS

open System.Collections.Generic
open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Linq
open Yard.Frontends.YardFrontend.Main
open Yard.Frontends.YardFrontend.GrammarParser
open Microsoft.VisualStudio.Language.Intellisense
open Yard.Core.Checkers
open System.Collections.Concurrent
open DataHelper

[<Export(typeof<ICompletionSourceProvider>)>]
[<ContentType("yardtype")>]
[<Name("YardCompletion")>]

type YardCompletionSource (buffer : ITextBuffer, m_dte : EnvDTE.DTE) =

    let dte =m_dte

    let mutable _disposed = false
    let theList = new ResizeArray<_>()
    let mutable l = List.empty

    let augmentCompletionSession (session : ICompletionSession) (completionSets : IList<CompletionSet>) = 
//        let tokens = ReParseFileForActiveWindow(m_dte, _sourceBuffer.CurrentSnapshot.GetText()).Tokens
//
//        let f = function 
//            | SEMICOLON _ -> true
//            | _ -> false
//
//        let rec getNumberOfSemicolumns = function
//            | [] -> 0
//            | h::t when f h -> 1 + getNumberOfSemicolumns t
//            | _::t -> getNumberOfSemicolumns t
//
//        let roolsNumber = getNumberOfSemicolumns tokens
//
//        //assume there was a change <=> number != roolsNumber
//        let position = session.TextView.Caret.Position.BufferPosition.Position
//
//        let less p = function
//            | SEMICOLON r when r.Start.AbsoluteOffset <= p -> true
//            | _ -> false
//
//        let lessSemi = List.filter (less position) tokens
//
//        let lessSemiPositions = List.map 
//                                    <| function
//                                       |(SEMICOLON r) -> r.End.AbsoluteOffset
//                                       | _ -> 0
//                                    <| lessSemi
//
//        let Start = lessSemiPositions.Last()
//
//        let greater p = function
//            | SEMICOLON r when r.Start.AbsoluteOffset >= p -> true
//            | _ -> false
//
//        let greaterSemi = List.filter (greater position) tokens
//
//        let greaterSemiPositions = List.map 
//                                    <| function
//                                       |(SEMICOLON r) -> r.End.AbsoluteOffset
//                                       | _ -> 0
//                                    <| greaterSemi
//
//        let End = greaterSemiPositions.[0]
//
//        let updatedText = session.TextView.TextBuffer.CurrentSnapshot.GetText()
//        let u = updatedText.[Start..End]

        let recomputeAllCompletions = 
            async{
                    let fileText = session.TextView.TextBuffer.CurrentSnapshot.GetText()
                    let getNonterminals (tree: Yard.Core.IL.Definition.t<_,_> ) = 
                      tree.grammar |> List.map (fun node -> node.name)
                    try 
                        let parsed = ReParseFileForActiveWindow(dte, fileText)                             // Запуск парсера
                        let getText (completion : Completion) = completion.DisplayText
               //         let r = (getNonterminals parsed).Distinct() |> List.ofSeq |> List.sort
                        let rHelper = parsed.NotTermToDEFPosition
                        let mutable r =  List.Empty
                        let rfun =  for kvp in parsed.NotTermToDEFPosition do r <- r @ [kvp.Key]
                        r <- List.sort r
                        let result = List.map (fun x -> new Completion(x)) r
                        lock theList (fun () -> theList.Clear(); theList.AddRange result)
                    with
                    | _ -> ()
                    if theList.Count > 0 then l <- List.ofArray <| theList.ToArray() }

        let returnCollection () =            
            let snapshot = buffer.CurrentSnapshot
            let triggerPoint = session.GetTriggerPoint(snapshot).GetValueOrDefault()
            //вычисляем место, где должен всплыть список
            let line = triggerPoint.GetContainingLine()
            let start = ref triggerPoint
            while (!start).Position > line.Start.Position
                    && not ( Char.IsWhiteSpace((!start - 1).GetChar()) ) do
                    start := !start - 1
            let x = new SnapshotSpan(!start, triggerPoint)
            let applicableTo = snapshot.CreateTrackingSpan(x.Span, SpanTrackingMode.EdgeInclusive)
            let mutable completionsList = theList.ToList()
            if theList.Count = 0 then completionsList <- l.ToList()
            completionSets.Add(new CompletionSet("All", "All", applicableTo, completionsList, Enumerable.Empty<Completion>()))
        //[recomputeAllCompletions]|> Async.Parallel  |> Async.RunSynchronously |> ignore
        recomputeAllCompletions |> Async.Start
        returnCollection ()
        
        
    let dispose () =  _disposed <- true

    interface ICompletionSource with
        member self.AugmentCompletionSession (x, y) = 
            try 
                augmentCompletionSession x y
            with
            |_-> ()
        member self.Dispose () = dispose ()


