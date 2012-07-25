namespace VSYardNS

open System
open System.ComponentModel.Composition
open System.Runtime.InteropServices
open System.Windows
open System.Windows.Input
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.Shell
open Yard.Frontends.YardFrontend.Main
open Yard.Frontends.YardFrontend.GrammarParser

type GoToDefKeyProcessor ( itsn : ITextStructureNavigator, view : ITextView) =
    inherit KeyProcessor ()

    let mutable _view : ITextView = null
    let mutable _navigator : ITextStructureNavigator = null
    do
        _view <- view
        _navigator <- itsn    
            
    override this.IsInterestedInHandledEvents : bool = true
    override this.KeyUp (args : KeyEventArgs) =
        let currentPoint = _view.Caret.Position.BufferPosition
        let mutable word : TextExtent = _navigator.GetExtentOfWord(currentPoint)

        if word.IsSignificant then

            let pos = ref 0
        
            let t = word.Span.GetText()

            let fileText = _view.TextBuffer.CurrentSnapshot.GetText()//u//buffer.CurrentSnapshot.GetText()

            let getNonterminals (tree: Yard.Core.IL.Definition.t<_,_> ) = 
                tree.grammar |> List.map (fun node -> node.name)
            try 
                let parsed = ParseText fileText ""  // Запуск парсера
                let nonterminals = (getNonterminals parsed)(*.Distinct()*) |> List.ofSeq |> List.sort
                let isCurrent str (nonterm : Yard.Core.IL.Source.t) = 
                    match nonterm with
                    | n, (s,e,_) when n = str -> pos := s
                    | _ -> ()
                List.iter (isCurrent t)  nonterminals
            with
            |_-> () 
            
            let lineNumberToGo = _view.TextBuffer.CurrentSnapshot.GetLineFromPosition(!pos).LineNumber
            let currentLineNumber = _view.TextSnapshot.GetLineNumberFromPosition _view.Caret.Position.BufferPosition.Position
            
            let rec scroll i = 
                match i with
                | 0 -> ()
                | n when n < 0 -> _view.ViewScroller.ScrollViewportVerticallyByLine(ScrollDirection.Up)
                                  scroll (n+1)
                | n when n > 0 -> _view.ViewScroller.ScrollViewportVerticallyByLine(ScrollDirection.Down)
                                  scroll (n-1)

            scroll (lineNumberToGo - currentLineNumber) //scrolling


                //Caret moving - begin


            _view.Caret.MoveTo(new SnapshotPoint(_view.TextSnapshot, !pos)) |> ignore

                //Caret moving end
            ()
  
        

    member this.SetView (v) = _view <- v
    member this.SetNavigator (n) = _navigator <- n


