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
    //let mutable m_braceList : Dictionary<Yard.Frontends.YardFrontend.GrammarParser.token,Yard.Frontends.YardFrontend.GrammarParser.token> = null
    
    let mutable lexeredText = null

    let getTags (spans : NormalizedSnapshotSpanCollection) =
        Seq.empty

    let TagsChanged = new Event<_>()
        

    let UpdateAtCaretPosition (position : CaretPosition) =
        CurrentChar <- position.Point.GetPoint(SourceBuffer, position.Affinity)
        //if not CurrentChar.HasValue then RETUTN                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
        TagsChanged.Trigger (self, new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0, SourceBuffer.CurrentSnapshot.Length)))
        ()    
    
    let CaretPositionChanged (sender: obj, e : CaretPositionChangedEventArgs) = 
        UpdateAtCaretPosition(e.NewPosition)      

    let ViewLayoutChanged (sender : obj, e : TextViewLayoutChangedEventArgs) =
        if not (e.OldSnapshot = e.NewSnapshot) then
            UpdateAtCaretPosition(View.Caret.Position)
    let mutable lexeredText = LexString (SourceBuffer.CurrentSnapshot.GetText())

    do //this.View.Caret.PositionChanged += CaretPositionChanged;    //this.View.LayoutChanged += ViewLayoutChanged;
        View <- view
        SourceBuffer <- sourceBuffer
        //Event.add CaretPositionChanged View.Caret.PositionChanged ///How??


    let FindMatchingCloseChar (start : SnapshotPoint, openChar : char, closeChar : char, maxLines : int, pairSpan : SnapshotSpan) = //return the value
        lexeredText <- LexString (SourceBuffer.CurrentSnapshot.GetText())
        let currentPosition = start.Position //shows the number of chars befor the cursor position
        let hasTheCurrentPosition t =
                match t with
                | LPAREN (x) -> (x.Start.AbsoluteOffset = currentPosition)
                | _ -> false
        let gotIt = Seq.exists hasTheCurrentPosition lexeredText
        let y = 
            lexeredText
            |> Seq.findIndex
                (fun t ->
                    match t with
                    | LPAREN (x) -> (x.Start.AbsoluteOffset = currentPosition)
                    | _ -> false)
        
    interface ITagger<TextMarkerTag> with
        member self.GetTags spans = getTags spans
        member self.add_TagsChanged x   = () // Event.add x TagsChanged.Publish
        member self.remove_TagsChanged x =  ()