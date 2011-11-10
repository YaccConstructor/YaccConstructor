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

type internal BraceMatchingTagger (view : ITextView, sourceBuffer : ITextBuffer) = class
     interface ITagger<ITag> with
      member self.GetTags spans = null
      member self.add_TagsChanged x = ()
      member self.remove_TagsChanged x = ()
       (* let View : ITextView = view
        let SourceBuffer : ITextBuffer = sourceBuffer
        let CurrentChar : Nullable<SnapshotPoint> = null
        let m_braceList : Dictionary<char, char> = new Dictionary<char,char>

        member this.ViewLayoutChanged(sender : System.Object, e : TextViewLayoutChangedEventArgs) = {}
        member this.CaretPositionChanged(sender : System.Object, e : CaretPositionChangedEventArgs) = {}
        member this.UpdateAtCaretPosition(caretPosition : CaretPosition) = {}            
        member this.GetTags(spans : NormalizedSnapshotSpanCollection) = {}*)
    end