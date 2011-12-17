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

type BraceMatchingTagger (view : ITextView, sourceBuffer : ITextBuffer) =
    let mutable View : ITextView = null
    let mutable SourceBuffer : ITextBuffer = null
    let mutable CurrentChar : Nullable<SnapshotPoint> = new Nullable<SnapshotPoint>()

    let getTags (spans : NormalizedSnapshotSpanCollection) =
        Seq.empty

    let TagsChanged = new Event<_>()

    let UpdateAtCaretPosition (position : CaretPosition) =
        CurrentChar <- position.Point.GetPoint(SourceBuffer, position.Affinity)
        let tempEvent = TagsChanged
        //if not (tempEvent = null)
           // then 
        tempEvent.Trigger (new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot,0,SourceBuffer.CurrentSnapshot.Length)))
                  
    let CaretPositionChanged (sender : obj) (e : CaretPositionChangedEventArgs) = 
        UpdateAtCaretPosition(e.NewPosition)
    let ViewLayoutChanged (sender: obj) (e : TextViewLayoutChangedEventArgs) =
        if not (e.OldSnapshot = e.NewSnapshot) then
            UpdateAtCaretPosition(View.Caret.Position)
    //let lexeredText = ref Seq.empty
//    let z = if not (SourceBuffer = null)
//                then
//                    lexeredText <- LexString (SourceBuffer.CurrentSnapshot.GetText())
//                else ()

    do 
        View <- view
        SourceBuffer <- sourceBuffer
        View.Caret.PositionChanged.AddHandler (new EventHandler<CaretPositionChangedEventArgs>(CaretPositionChanged))
        View.LayoutChanged.AddHandler (new EventHandler<TextViewLayoutChangedEventArgs>(ViewLayoutChanged))


    let FindMatchingCloseChar (start : SnapshotPoint, openChar : char, closeChar : char, maxLines : int) = 
        let currentPosition = start.Position //shows the number of chars before the cursor position
        let isParenthesisToken t =
            match t with
            | LPAREN x 
            | RPAREN x -> x.Start.AbsoluteOffset > currentPosition
            | _ -> false
        let count = ref 0
        let parentheses = List.ofSeq (LexString (SourceBuffer.CurrentSnapshot.GetText()) |> Seq.filter isParenthesisToken) //got all parentheses after the first
        let w = ref -1
        let got = ref false
        let s x =
            match x with
            | LPAREN _ -> count:= !count + 1
            | RPAREN _ when not (!count = 0) -> count := !count - 1
            | RPAREN r when (!count = 0) && (!got = false) -> w := r.Start.AbsoluteOffset 
            | _ -> ()
            if not (!w = -1) then got := true

        List.iter s parentheses
        let resultPoint = new SnapshotPoint (start.Snapshot, !w)
        let pairSpan = new SnapshotSpan(resultPoint, 1)
        pairSpan

    let FindMatchingOpenChar (finish : SnapshotPoint, openChar : char, closeChar : char, maxLines : int) = 
        let currentPosition = finish.Position //shows the number of chars before the cursor position
        let isParenthesisToken t =
            match t with
            | LPAREN x 
            | RPAREN x -> x.Start.AbsoluteOffset < currentPosition
            | _ -> false
        let count = ref 0
        let parentheses = List.rev (List.ofSeq (LexString (SourceBuffer.CurrentSnapshot.GetText()) |> Seq.filter isParenthesisToken)) //got all parentheses before the last (from the closest)
        let w = ref -1
        let got = ref false
        let s x =
            match x with
            | RPAREN _ -> count:= !count + 1
            | LPAREN _ when not (!count = 0) -> count := !count - 1
            | LPAREN r when (!count = 0) && (!got = false) -> w := r.Start.AbsoluteOffset 
            | _ -> ()
            if not (!w = -1) then got := true

        List.iter s parentheses
        let resultPoint = new SnapshotPoint (finish.Snapshot, !w)
        let pairSpan = new SnapshotSpan(resultPoint, 1)
        pairSpan
    
    let getTags (spans : NormalizedSnapshotSpanCollection) =
        let ok = not (spans.Count = 0) && CurrentChar.HasValue && (CurrentChar.Value.Position< CurrentChar.Value.Snapshot.Length) 
        let result = if ok
                        then
                            let currentChar = ref CurrentChar.Value
                            if not (spans.ElementAt<SnapshotSpan>(0).Snapshot = (!currentChar).Snapshot) then
                                        currentChar := (!currentChar).TranslateTo(spans.ElementAt<SnapshotSpan>(0).Snapshot, PointTrackingMode.Positive)
                            let currentText = (!currentChar).GetChar()
                            if currentText = '(' || currentText = ')' then
                                let lastChar = if (!currentChar).Position = 0
                                                    then
                                                        !currentChar
                                                    else (!currentChar) - 1
                                let lastText = lastChar.GetChar()
                                let pairSpan = 
                                    if (currentText = '(')
                                        then FindMatchingCloseChar(!currentChar, currentText, ')', View.TextViewLines.Count)
                                        else if currentText = ')'
                                            then FindMatchingOpenChar(!currentChar, currentText, '(', View.TextViewLines.Count)
                                            else (new SnapshotSpan())
                                let s = (new TagSpan<TextMarkerTag>(new SnapshotSpan(!currentChar,1),new TextMarkerTag("green"))) :> ITagSpan<TextMarkerTag>
                                let e = (new TagSpan<TextMarkerTag>(pairSpan,new TextMarkerTag("green"))) :> ITagSpan<TextMarkerTag>
                                let preresult = seq { yield s; yield e}
                                preresult
                            else Seq.empty
                        else Seq.empty
        result

    interface ITagger<TextMarkerTag> with
        member self.GetTags spans = getTags spans
        member self.add_TagsChanged x =
           Event.add (fun arg -> x.Invoke(self,arg)) TagsChanged.Publish
        member self.remove_TagsChanged x =
            TagsChanged.Publish.RemoveHandler ( new Handler<SnapshotSpanEventArgs> (fun s a -> x.Invoke(s,a) ) )