namespace VSYardNS
//
//open System
//open System.Collections.Generic
//open System.ComponentModel.Composition
//open Microsoft.VisualStudio.Text
//open Microsoft.VisualStudio.Text.Classification
//open Microsoft.VisualStudio.Text.Editor
//open Microsoft.VisualStudio.Text.Tagging
//open Microsoft.VisualStudio.Utilities
//open System.Linq
//
//type YardClassifier (buffer, yardTagAggregator, typeService:IClassificationTypeRegistryService) = class
//    let mutable _aggregator:ITagAggregator<TokenTag> = null
//    do _aggregator <- yardTagAggregator
//    let yardTypes = 
//        [
//            TNonterm, typeService.GetClassificationType("ynterm")
//            TTerm, typeService.GetClassificationType("yterminal")
//            TLiteral, typeService.GetClassificationType("yliteral")
//            TComment, typeService.GetClassificationType("ycomment")
//            TOther, typeService.GetClassificationType("yother")
//        ]
//        |> dict
//    let getTags (spans:NormalizedSnapshotSpanCollection) = 
//        _aggregator.GetTags(spans)
//        |> Seq.map 
//            (fun tagSpan ->
//                let tagSpans = tagSpan.Span.GetSpans(spans.[0].Snapshot)                
//                new TagSpan<ClassificationTag>(tagSpans.[0], new ClassificationTag(yardTypes.[tagSpan.Tag.Type]))
//            )
//        
//    interface ITagger<ClassificationTag> with
//        member self.GetTags spans = getTags spans :?> _
//        member self.add_TagsChanged x = ()
//        member self.remove_TagsChanged x = ()
// end