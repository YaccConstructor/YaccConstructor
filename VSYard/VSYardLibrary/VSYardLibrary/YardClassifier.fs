module YardClassifier
open System
open System.Collections.Generic
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Linq;

[<Export(typeof<ITaggerProvider>)>]
[<ContentType("yard")>]
[<TagType(typeof<ClassificationTag>)>]
type internal YardClassifier () = class
        [<Import>]
        let mutable aggregatorFactory = null
            member self.AggregatorFactory 
                with 
                    get () = aggregatorFactory
                    set af = aggregatorFactory <- af

        interface ITaggerProvider with
            member this. CreateTagger<'T when 'T :> ITag>(buffer : ITextBuffer) : ITagger<'T> =
                let ookTagAggregator : ITagAggregator<TokenTag> = aggregatorFactory.CreateTagAggregator<TokenTag>(buffer)
                MyClassifier(buffer, ookTagAggregator, ClassificationTypeRegistry) :> ITagger<'T>; 
    end
