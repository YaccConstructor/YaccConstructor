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


type BraceMatchingTagger (view : ITextView, sourceBuffer : ITextBuffer) =
    let mutable View : ITextView = null
    let mutable SourceBuffer : ITextBuffer = null
    let mutable CurrentChar : Nullable<SnapshotPoint> = new Nullable<SnapshotPoint>() //is this right?
//    let mutable CurrenToken : Yard.Frontends.YardFrontend.GrammarParser.token = Yard.Frontends.YardFrontend.GrammarParser.LPAREN (new Yard.Frontends.YardFrontend.GrammarParser.Range(1,2))
//    let mutable m_braceList : Dictionary<Yard.Frontends.YardFrontend.GrammarParser.token,Yard.Frontends.YardFrontend.GrammarParser.token> = null
    let mutable lexeredText = null

// *    let TagsChanged : Event<SnapshotSpanEventArgs> = new Event<SnapshotSpanEventArgs>() //how should this be done?
//    let UpdateAtCaretPosition caretPosition : CaretPosition = 
//        CurrentChar <- caretPosition.Point.GetPoint(SourceBuffer, caretPosition.Affinity)
//        if not CurrentChar.HasValue then ()
//            else
//                let tempEvent = TagsChanged
//                if not (tempEvent.Equals(null)) then tempEvent.Trigger(new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0,SourceBuffer.CurrentSnapshot.Length)))
//    let CaretPositionChanged (sender : obj, e : CaretPositionChangedEventArgs) = 
//        UpdateAtCaretPosition(e.NewPosition)
//          ()
//                
//    let ViewLayoutChanged (sender : obj, e : TextViewLayoutChangedEventArgs) = 
//        if e.NewSnapshot = e.OldSnapshot then UpdateAtCaretPosition(View.Caret.Position)
//        ()
    do
        //m_braceList.Add(Yard.Frontends.YardFrontend.GrammarParser.LPAREN,Yard.Frontends.YardFrontend.GrammarParser.RPAREN)
        View <- view
        SourceBuffer <- sourceBuffer
// *       View.Caret.PositionChanged.Add(CaretPositionChanged)
//        View.LayoutChanged.Add(ViewLayoutChanged)
//        if not (View.ToString() = "") then lexeredText <- Yard.Frontends.YardFrontend.Main.LexString (View.ToString())

    let getTags (spans : NormalizedSnapshotSpanCollection) =
        if spans.Count = 0 then ()

//        if not ((not CurrentChar.HasValue) || CurrentChar.Value.Position >= CurrentChar.Value.Snapshot.Length)&& (not (spans.Count = 0) ) then 
//            let mutable currentChar : SnapshotPoint = CurrentChar.Value
//        
//            if not (spans.ElementAt(0).Snapshot = currentChar.Snapshot) then                   //is replacing spans[0] to what is here correct?
//                currentChar <- currentChar.TranslateTo(spans.ElementAt(0).Snapshot, PointTrackingMode.Positive)
//
//            let mutable currentText : char = currentChar.GetChar()
//            let mutable lastChar : SnapshotPoint = 
//                if currentChar.Position = 0 then currentChar
//                    else currentChar - 1
//            let mutable lastText = lastChar.GetChar()
//            let mutable pairSpan = new SnapshotSpan()
//
//    //        if currentText = Yard.Frontends.YardFrontend.GrammarParser.LPAREN then
//    //            let closeChar = ')'
//
//            let x = Yard.Frontends.YardFrontend.Main.LexString (View.ToString())
//            ()

        Seq.empty
    
        
    interface ITagger<TextMarkerTag> with
        member self.GetTags spans = getTags spans
        member self.add_TagsChanged x = ()
        member self.remove_TagsChanged x = ()