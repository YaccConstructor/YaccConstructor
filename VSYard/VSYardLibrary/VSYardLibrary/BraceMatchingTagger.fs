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
open DataHelper

type BraceMatchingTagger (view : ITextView, sourceBuffer : ITextBuffer, m_dte : EnvDTE.DTE) =
    let dte = m_dte
    let mutable View : ITextView = null
    let mutable SourceBuffer : ITextBuffer = null
    let mutable CurrentChar : Nullable<SnapshotPoint> = new Nullable<SnapshotPoint>()

    let TagsChanged = new Event<_>()

    let UpdateAtCaretPosition (position : CaretPosition) =
        CurrentChar <- position.Point.GetPoint(SourceBuffer, position.Affinity)
        let tempEvent = TagsChanged
        tempEvent.Trigger (new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot,0,SourceBuffer.CurrentSnapshot.Length)))
                  
    let CaretPositionChanged (sender : obj) (e : CaretPositionChangedEventArgs) = 
        UpdateAtCaretPosition(e.NewPosition)

    let ViewLayoutChanged (sender: obj) (e : TextViewLayoutChangedEventArgs) =
        if not (e.OldSnapshot = e.NewSnapshot) && ( e.NewSnapshot.Length = e.OldSnapshot.Length || e.VerticalTranslation || e.HorizontalTranslation) then
            UpdateAtCaretPosition(View.Caret.Position)

    do 
        View <- view
        SourceBuffer <- sourceBuffer
        View.Caret.PositionChanged.AddHandler (new EventHandler<CaretPositionChangedEventArgs>(CaretPositionChanged))
        View.LayoutChanged.AddHandler (new EventHandler<TextViewLayoutChangedEventArgs>(ViewLayoutChanged))

    let FindMatchForOpen (parentheses, openParenthesisPosition) = 
        let isFurtherThanCurrent x =
            match x with
            |LPAREN r when r.Start.AbsoluteOffset > openParenthesisPosition -> true
            |RPAREN r when r.Start.AbsoluteOffset > openParenthesisPosition -> true
            | _ -> false

        let furtherParentheses = List.filter isFurtherThanCurrent parentheses
        let closeParenthesisPosition = ref -1
        let got = ref false
        let count = ref 0
        let counter x =
            match x with
            |LPAREN _ -> count := !count + 1
            |RPAREN _ when not (!count = 0 ) -> count:= !count - 1
            |RPAREN r when (!count = 0) && not !got -> closeParenthesisPosition := r.Start.AbsoluteOffset
            | _ -> ()
            if not (!closeParenthesisPosition = -1) then got := true
        List.iter counter furtherParentheses
        let result = ref (new SnapshotSpan())
        if !got then result := new SnapshotSpan (new SnapshotPoint (SourceBuffer.CurrentSnapshot, !closeParenthesisPosition), 1)
        !result

    let FindMatchForClose (parentheses, closeParenthesisPosition) = 
        let isFurtherThanCurrent x =
            match x with
            |LPAREN r when r.Start.AbsoluteOffset < closeParenthesisPosition -> true
            |RPAREN r when r.Start.AbsoluteOffset < closeParenthesisPosition -> true
            | _ -> false
        let furtherParentheses = List.rev <| List.filter isFurtherThanCurrent parentheses
        let openParenthesisPosition = ref -1
        let got = ref false
        let count = ref 0
        let counter x =
            match x with
            |RPAREN _ -> count := !count + 1
            |LPAREN _ when not (!count = 0) -> count := !count - 1
            |LPAREN r when (!count = 0) && not !got -> openParenthesisPosition := r.Start.AbsoluteOffset
            | _ -> ()
            if not (!openParenthesisPosition = -1) then got := true
        List.iter counter furtherParentheses
        let result = ref (new SnapshotSpan())
        if !got then result := new SnapshotSpan (new SnapshotPoint (SourceBuffer.CurrentSnapshot, !openParenthesisPosition), 1)
        !result
    
    let getTags (spans : NormalizedSnapshotSpanCollection) =
        let isLParen x = 
            match x with
            |LPAREN _ -> true
            | _ -> false
        let isRParen x = 
            match x with
            |RPAREN _ -> true
            | _ -> false
        let isParenthesis x = isRParen x || isLParen  x
        let ok = not (spans.Count = 0) && CurrentChar.HasValue && (CurrentChar.Value.Position< CurrentChar.Value.Snapshot.Length) 
        let result = 
            if ok
            then
                let currentChar = ref CurrentChar.Value

                if not (spans.ElementAt<SnapshotSpan>(0).Snapshot = (!currentChar).Snapshot) then
                            currentChar := (!currentChar).TranslateTo(spans.ElementAt<SnapshotSpan>(0).Snapshot, PointTrackingMode.Positive)
                           
                let currentPosition = currentChar.Value.Position
                let lexeredText = ref List.Empty
                try
                 (* let activeSolutionProjects = dte.ActiveSolutionProjects :?> Array
                    let activeProject =  activeSolutionProjects.GetValue(0) :?> EnvDTE.Project
                    let yaFile = dte.ActiveDocument :?> EnvDTE.Document
                    let solution1 = SolutionData.GetSolution()
                    let projectFileName = activeProject.Properties.Item("FileName").Value.ToString()
                    let yardFileName = yaFile.Name
                    lexeredText := (solution1.ReParseFile(projectFileName,yardFileName, SourceBuffer.CurrentSnapshot.GetText())).Tokens *)
                    lexeredText := ReParseFileInActiveWindow(m_dte, SourceBuffer.CurrentSnapshot.GetText()).Tokens
                    // lexeredText := List.ofSeq <| LexString ( SourceBuffer.CurrentSnapshot.GetText() )
                    // Вставить всё сюда
                with
                |_ -> ()
                let parentheses = List.filter isParenthesis !lexeredText
                let shouldBeProcessed = 
                    parentheses 
                    |> List.exists (function
                                    |LPAREN r
                                    |RPAREN r when r.Start.AbsoluteOffset = currentPosition -> true
                                    | _                                                     -> false)

                if shouldBeProcessed
                    then
                        let currentToken = parentheses.ElementAt(0) |> ref
                        let f x =
                            match x with
                            |LPAREN r
                            |RPAREN r when r.Start.AbsoluteOffset = currentPosition -> currentToken := x 
                            | _ -> ()
                        List.iter f parentheses
                        let pairSpan = 
                            if isLParen !currentToken
                                then FindMatchForOpen (parentheses, currentPosition)
                                elif isRParen !currentToken
                                    then FindMatchForClose(parentheses, currentPosition)
                                    else (new SnapshotSpan())
                        let makeTagSpan span = (new TagSpan<TextMarkerTag>(span, new TextMarkerTag("green"))) :> ITagSpan<TextMarkerTag>
                        let s = new SnapshotSpan(!currentChar,1) |> makeTagSpan
                        let e = makeTagSpan pairSpan
                        let preresult = seq [s; e]
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