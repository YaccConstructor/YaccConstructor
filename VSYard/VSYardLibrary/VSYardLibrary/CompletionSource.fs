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

[<Export(typeof<ICompletionSourceProvider>)>]
[<ContentType("text")>]
[<Name("YardCompletion")>]

type YardCompletionSource (buffer : ITextBuffer) =
    let mutable _disposed = false
    let theList = new ResizeArray<_>()

    let augmentCompletionSession (session : ICompletionSession) (completionSets : IList<CompletionSet>) = 
        let recomputeAllCompletions = 
            async{
            let fileText = buffer.CurrentSnapshot.GetText()
            let getNonterminals (tree: Yard.Core.IL.Definition.t<_,_> ) = 
                tree.grammar |> List.map (fun node -> node.name)
            try 
                let parsed = ParseText fileText
                let getText (completion : Completion) = completion.DisplayText
                let r = (getNonterminals parsed).Distinct() |> List.ofSeq |> List.sort
                let result = List.map (fun x -> new Completion(x)) r
                lock theList (fun () -> theList.Clear(); theList.AddRange result)
            with
            | _ -> ()}
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
            let completionsList = theList.ToList()
            completionSets.Add(new CompletionSet("All", "All", applicableTo, completionsList, Enumerable.Empty<Completion>()))
        Async.Start recomputeAllCompletions
        returnCollection ()
        
    let dispose () = 
        _disposed <- true
    interface ICompletionSource with
        member self.AugmentCompletionSession (x, y) = augmentCompletionSession x y
        member self.Dispose () = dispose ()

type YardCompletionSourceProvider ( textBuffer : ITextBuffer) = 
    interface ICompletionSourceProvider with
     member self.TryCreateCompletionSource textBuffer = 
        new YardCompletionSource(textBuffer) :> ICompletionSource
