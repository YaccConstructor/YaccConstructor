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

type BraceMatchingTagger (view : ITextView, sourceBuffer : ITextBuffer) as self =
    let mutable View : ITextView = null
    let mutable SourceBuffer : ITextBuffer = null
    let mutable CurrentChar : Nullable<SnapshotPoint> = new Nullable<SnapshotPoint>() //is this right?
    let mutable lexeredText = null

    let getTags (spans : NormalizedSnapshotSpanCollection) =
        Seq.empty

    let TagsChanged = new Event<_>()
        

    let UpdateAtCaretPosition (position : CaretPosition) =
        CurrentChar <- position.Point.GetPoint(SourceBuffer, position.Affinity)
        TagsChanged.Trigger (new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0, SourceBuffer.CurrentSnapshot.Length)))
        ()    
    let CaretPositionChanged (sender : obj) (e : CaretPositionChangedEventArgs) = 
        UpdateAtCaretPosition(e.NewPosition)
    let ViewLayoutChanged (sender: obj) (e : TextViewLayoutChangedEventArgs) =
        if not (e.OldSnapshot = e.NewSnapshot) then
            UpdateAtCaretPosition(View.Caret.Position)
    let mutable lexeredText = LexString (SourceBuffer.CurrentSnapshot.GetText())

    do 
        View <- view
        SourceBuffer <- sourceBuffer
        View.Caret.PositionChanged.AddHandler (new EventHandler<CaretPositionChangedEventArgs>(CaretPositionChanged))
        View.LayoutChanged.AddHandler (new EventHandler<TextViewLayoutChangedEventArgs>(ViewLayoutChanged))


    let FindMatchingCloseChar (start : SnapshotPoint, openChar : char, closeChar : char, maxLines : int, pairSpan : SnapshotSpan) = //return the value
        lexeredText <- LexString (SourceBuffer.CurrentSnapshot.GetText())
        let currentPosition = start.Position //shows the number of chars before the cursor position
        let hasTheCurrentPosition t =
                match t with
                | LPAREN (x) -> (x.Start.AbsoluteOffset = currentPosition)
                | _ -> false
        let isParenthesisToken t =
            match t with
            | LPAREN (x) -> (x.Start.AbsoluteOffset > currentPosition)
            | RPAREN (x) -> (x.Start.AbsoluteOffset > currentPosition)
            | _ -> false
        let gotIt = Seq.exists hasTheCurrentPosition lexeredText
        let y = 
            lexeredText
            |> Seq.findIndex
                (fun t ->
                    match t with
                    | LPAREN (x) -> (x.Start.AbsoluteOffset = currentPosition)
                    | _ -> false)
        let count = ref 0
        let parentheses = Seq.filter isParenthesisToken lexeredText //got all parentheses after the first
        let index = ref 0
        let checkTheItem (x : token) =
            match x with
                | LPAREN (r) -> (count := !count + 1)
                | RPAREN (r) -> if not (!count = 0) then (count := !count - 1)
                | _ -> ()
        for i = 0 to Seq.length parentheses - 1 do
            let t = Seq.head parentheses
            checkTheItem t
            if !count = 0 then
                match t with
                    | RPAREN (r) -> index := r.Start.AbsoluteOffset
                    | _ -> () //potential risk, if got there then sth went wrong
        done
        ()

    interface ITagger<TextMarkerTag> with
        member self.GetTags spans = getTags spans
        member self.add_TagsChanged z  = //no idea of any way of transforming EventHandler<SnapshotSpanEvebtArgs> to acceptible type for Event.add or TagsChanged.Add
           // TagsChanged.Publish.AddHandler z
            () // Event.add x TagsChanged.Publish
        member self.remove_TagsChanged x =  ()