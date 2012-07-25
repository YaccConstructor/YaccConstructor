namespace VSYardNS

open System.Collections.Generic
open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Text.Operations
open System.Linq
open Yard.Frontends.YardFrontend.Main
open Yard.Frontends.YardFrontend.GrammarParser
open System.Threading
open System.Windows.Media


type HighlightWordTag () = 
    inherit TextMarkerTag ("MarkerFormatDefinition/HighlightWordFormatDefinition")
    //new () = { inherit TextMarkerTag ("MarkerFormatDefinition/HighlightWordFormatDefinition") }
    //do ()


type HighlightWordTagger (view : ITextView, sourceBuffer : ITextBuffer, textSearchService : ITextSearchService, textStructureNavigator : ITextStructureNavigator) =
    let TagsChanged = new Event<_>()
    let mutable _view : ITextView = null
    let mutable _sourceBuffer : ITextBuffer = null
    let mutable _textSearchService : ITextSearchService = null
    let mutable _textStructureNavigator : ITextStructureNavigator = null
    let mutable _wordSpans = new NormalizedSnapshotSpanCollection ()
    let mutable _currentWord = new Nullable<SnapshotSpan> () //was nullable
    let mutable _requestedPoint = new SnapshotPoint ()
    let mutable _updateLock = new obj() 

    let FindNewSpans (w : string) = 
        let (spans : SnapshotSpan list ref) = ref List.empty 
        let fileText = _view.TextBuffer.CurrentSnapshot.GetText()//u//buffer.CurrentSnapshot.GetText()

        let getNonterminals (tree: Yard.Core.IL.Definition.t<_,_> ) = 
            tree.grammar |> List.map (fun node -> node.name)

        try 
            let parsed = ParseText fileText ""  // Запуск парсера
            let nonterminals = (getNonterminals parsed).Distinct() |> List.ofSeq |> List.sort
            //finding all ocurrences
            let lexered = LexString fileText |> List.ofSeq
            let checkAndAdd (t:token) = 
                match t with
                | LIDENT (name, (s,e,_))  when name = w -> 
                                                           spans:= !spans @ [new SnapshotSpan(_view.TextBuffer.CurrentSnapshot, new Span(s, e - s ))]  //to redo line
                | _ -> ()
            List.iter checkAndAdd lexered
        with
        | _ -> ()
        !spans
    let WordExtentIsValid (currentRequest:SnapshotPoint , word:TextExtent) = 
        let rec isAllLeters (str : string)= 
            match str with
            | "" -> true
            | s -> Char.IsLetter(s.[0]) && isAllLeters(s.[1..s.Length-1])
        word.IsSignificant && isAllLeters <| currentRequest.Snapshot.GetText(new Span(word.Span.Start.Position, word.Span.End.Position))
    let SynchronousUpdate (currentRequest:SnapshotPoint, newSpans:NormalizedSnapshotSpanCollection, newCurrentWord:Nullable<SnapshotSpan>) = 
        let f () = 
            if (currentRequest = _requestedPoint)
                then 
                    _wordSpans <- newSpans
                    _currentWord <- newCurrentWord
                    let tempEvent = TagsChanged
                    tempEvent.Trigger(new SnapshotSpanEventArgs(new SnapshotSpan(_sourceBuffer.CurrentSnapshot, 0, _sourceBuffer.CurrentSnapshot.Length)))
        lock _updateLock f

    let UpdateWordAdornments () = 
        let currentRequest = _requestedPoint
        let mutable wordSpans = List.empty
        //Find all words in the buffer like the one the caret is on
        let mutable word : TextExtent = _textStructureNavigator.GetExtentOfWord(currentRequest)
        let mutable foundWord = true

        let mutable flag = true //This has appeared here just for helping in re-writing C# into F# (I needed a "return" at about 99 line, after SynchronousUpdate call

        //If we've selected something not worth highlighting, we might have missed a "word" by a little bit
//        if not (WordExtentIsValid(currentRequest, word))
//            then
//                //Before we retry, make sure it is worthwhile
//                if (not (word.Span.Start = currentRequest) || currentRequest = currentRequest.GetContainingLine().Start || Char.IsWhiteSpace((currentRequest - 1).GetChar()))
//                    then
//                        foundWord <- false
//                    else
//                        // Try again, one character previous. 
//                        //If the caret is at the end of a word, pick up the word.
//                        word <- _textStructureNavigator.GetExtentOfWord(currentRequest - 1)
//                        //If the word still isn't valid, we're done
//                        if not (WordExtentIsValid(currentRequest, word))
//                            then foundWord <- false 
        if ( not foundWord)
            then 
                //If we couldn't find a word, clear out the existing markers
                SynchronousUpdate(currentRequest, new NormalizedSnapshotSpanCollection(), new Nullable<SnapshotSpan>())
                flag <- false

        if flag then 
            let currentWord = word.Span
            if flag && _currentWord.HasValue && currentWord = _currentWord.Value then //was check for _currentWord.HasValue
                //If this is the current word, and the caret moved within a word, we're done
                flag <- false

            if flag then
                //searching all positins of the word
                //let a = new SnapshotSpan(new SnapshotPoint(_view.TextSnapshot, 13), 9)
                //let b = new SnapshotSpan(new SnapshotPoint(_view.TextSnapshot, 38), 9)
                let t = word.Span.GetText()
                let x =  FindNewSpans  t
                wordSpans <- wordSpans @ x

                //START
                let pos = ref 0

                let fileText = _view.TextBuffer.CurrentSnapshot.GetText()//u//buffer.CurrentSnapshot.GetText()

                let getNonterminals (tree: Yard.Core.IL.Definition.t<_,_> ) = 
                    tree.grammar |> List.map (fun node -> node.name)
                try 
                    let parsed = ParseText fileText ""  // Запуск парсера
                    let nonterminals = (getNonterminals parsed).Distinct() |> List.ofSeq |> List.sort
                    let isCurrent str (nonterm : Yard.Core.IL.Source.t) = 
                       match nonterm with
                       | n, (s,e,_) when n = str -> pos := s
                       | _ -> ()
                    List.iter (isCurrent t)  nonterminals
                with
                |_-> ()
                let lineNumberToGo = _view.TextBuffer.CurrentSnapshot.GetLineFromPosition(!pos).LineNumber
                let currentLineNumber = _view.TextSnapshot.GetLineNumberFromPosition _view.Caret.Position.BufferPosition.Position
                //let positionFromTopToGo = _view.LineHeight * (lineNumberToGo |> float)

                let rec scroll i = 
                    match i with
                    | 0 -> ()
                    | n when n < 0 -> _view.ViewScroller.ScrollViewportVerticallyByLine(ScrollDirection.Up)
                                      scroll (n+1)
                    | n when n > 0 -> _view.ViewScroller.ScrollViewportVerticallyByLine(ScrollDirection.Down)
                                      scroll (n-1)

                scroll (lineNumberToGo - currentLineNumber)
                ()


                //Caret moving - begin


                if not x.IsEmpty then
                    _view.Caret.MoveTo(new SnapshotPoint(_view.TextSnapshot, !pos)) |> ignore

                //Caret moving end

            if (currentRequest = _requestedPoint) then
                SynchronousUpdate(currentRequest, new NormalizedSnapshotSpanCollection(wordSpans), new Nullable<_>(currentWord))
                    
    let UpdateAtCaretPosition (caretPosition : CaretPosition) = 
        let point = caretPosition.Point.GetPoint(_sourceBuffer, caretPosition.Affinity)
        if not (not point.HasValue && _currentWord.HasValue
                && _currentWord.Value.Snapshot = _view.TextSnapshot
                && point.Value.Position >= _currentWord.Value.Start.Position //".Position" added
                && point.Value.Position <= _currentWord.Value.End.Position)
            then 
                _requestedPoint <- point.Value
                UpdateWordAdornments ()
    let ViewLayoutChanged (sender : obj) (e : TextViewLayoutChangedEventArgs) =
        if not (e.NewSnapshot = e.OldSnapshot)
                then UpdateAtCaretPosition _view.Caret.Position
    let CaretPositionChanged (sender : obj) (e : CaretPositionChangedEventArgs) =
        UpdateAtCaretPosition(e.NewPosition)

    do
        _view <- view
        _sourceBuffer <- sourceBuffer
        _textSearchService <- textSearchService;
        _textStructureNavigator <- textStructureNavigator
        _view.Caret.PositionChanged.AddHandler (new EventHandler<CaretPositionChangedEventArgs>(CaretPositionChanged))
        _view.LayoutChanged.AddHandler (new EventHandler<TextViewLayoutChangedEventArgs>(ViewLayoutChanged))
    let getTags (spans : NormalizedSnapshotSpanCollection) =
    //tmp:
        //if (_wordSpans.Count = 0 ) then _wordSpans <- spans
        let mutable shouldBeProcessed = true //!!!

        let mutable currentWord = _currentWord


        let mutable wordSpans = _wordSpans

        shouldBeProcessed <- shouldBeProcessed && (spans.Count > 0) && (wordSpans.Count > 0) && _currentWord.HasValue

        let mutable preresult = List.empty
        let mutable result = Seq.empty

        if shouldBeProcessed then
            if not (spans.[0].Snapshot = wordSpans.[0].Snapshot) then
                let spansList = List.ofArray <| wordSpans.ToArray() //there should be a smarter way to do that. (List != list)
                let f (span : SnapshotSpan) = span.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeInclusive)
                wordSpans <- new NormalizedSnapshotSpanCollection(Seq.ofList <| (List.map  f spansList))
                currentWord <- new Nullable<SnapshotSpan>(currentWord.Value.TranslateTo(spans.[0].Snapshot, SpanTrackingMode.EdgeInclusive))
            if spans.OverlapsWith(new NormalizedSnapshotSpanCollection(currentWord.Value)) then
                preresult <- preresult @ [(new TagSpan<HighlightWordTag>(currentWord.Value, new HighlightWordTag())):>ITagSpan<HighlightWordTag>]
            let Spans = NormalizedSnapshotSpanCollection.Overlap(spans, wordSpans).ToArray() |> List.ofArray
            let makeTag span = (new TagSpan<HighlightWordTag>(span, new HighlightWordTag()) :> ITagSpan<HighlightWordTag>)
            preresult <- preresult @ List.map makeTag Spans
            result <- Seq.ofList preresult
        result
    interface ITagger<HighlightWordTag> with
        member self.GetTags spans = getTags spans
        member self.add_TagsChanged x =
            Event.add (fun arg -> x.Invoke(self,arg)) TagsChanged.Publish
        member self.remove_TagsChanged x =
            TagsChanged.Publish.RemoveHandler ( new Handler<SnapshotSpanEventArgs> (fun s a -> x.Invoke(s,a) ) )

