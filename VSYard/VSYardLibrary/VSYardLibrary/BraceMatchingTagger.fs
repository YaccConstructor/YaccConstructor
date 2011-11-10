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
    let getTags (spans : NormalizedSnapshotSpanCollection) =
        //Здесь логика по подсветке скобок
        let res = Seq.empty
        res
        
    interface ITagger<ITag> with
        member self.GetTags spans = getTags spans
        member self.add_TagsChanged x = ()
        member self.remove_TagsChanged x = ()