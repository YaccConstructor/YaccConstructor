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

[<Export(typeof<ICompletionSourceProvider>)>]
[<ContentType("text")>]
[<Name("YardCompletion")>]

type YardCompletionSource (buffer : ITextBuffer) =
    let mutable _buffer : ITextBuffer = null
    let mutable _disposed = false
    do
        _buffer <- buffer
    let augmentCompletionSession (session : ICompletionSession) (completionSets : IList<CompletionSet>) = 
        //there was a check
        let fileText = _buffer.CurrentSnapshot.GetText()       

        let parsered = ParseText fileText
        let getNonterminals (tree : Yard.Core.IL.Definition.t<Yard.Core.IL.Source.t,Yard.Core.IL.Source.t> ) = 
            let getNameCompletion (node : Yard.Core.IL.Rule.t<Yard.Core.IL.Source.t,Yard.Core.IL.Source.t>) = 
                new Completion(node.name)
            List.map getNameCompletion tree.grammar
        //let temp = reachableRulesInfo parsered
        let completions = 
            let getText (completion : Completion) = completion.DisplayText
            List.sortBy getText (getNonterminals parsered)

        
        let snapshot = _buffer.CurrentSnapshot
        let triggerPoint = session.GetTriggerPoint(snapshot).GetValueOrDefault() //ok?
        //check that triggerPoint != null
        let line = triggerPoint.GetContainingLine()
        let start = ref triggerPoint
        while (!start).Position > line.Start.Position  && not ( Char.IsWhiteSpace((!start - 1).GetChar()) ) do //why doesn't work without impicit getting POSITION?
            start := !start - 1
        //let applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(!start, triggerPoint), SpanTrackingMode.EdgeInclusive)//wtf? it works in C#
        let x = new SnapshotSpan(!start, triggerPoint)
        let applicableTo = snapshot.CreateTrackingSpan(x.Span, SpanTrackingMode.EdgeInclusive)//wtf? it works in C#;
        completionSets.Add(new CompletionSet("All", "All", applicableTo, completions, Enumerable.Empty<Completion>()))

    let dispose () = 
        _disposed <- true
    interface ICompletionSource with
        member self.AugmentCompletionSession (x, y) = augmentCompletionSession x y
        member self.Dispose () = dispose ()

type YardCompletionSourceProvider ( textBuffer : ITextBuffer) = 
    interface ICompletionSourceProvider with
     member self.TryCreateCompletionSource textBuffer = 
        new YardCompletionSource(textBuffer) :> ICompletionSource
