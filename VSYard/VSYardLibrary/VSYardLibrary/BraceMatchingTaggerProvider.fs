namespace VSYardNS
open System
open System.Collections.Generic
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Linq

type internal BraceMatchingTaggerProvider = class
    interface IViewTaggerProvider with
        member self.CreateTagger (textView : ITextView, buffer : ITextBuffer) = 
            box (new BraceMatchingTagger(textView, buffer)) :?> ITagger<_>
end
