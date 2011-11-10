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

type internal BraceMatchingTaggerProvider<'T> = class
    interface IViewTaggerProvider with
        member self.CreateTagger<'T> (textView : ITextView, buffer : ITextBuffer) = 
            new BraceMatchingTagger(textView, buffer) 
end
