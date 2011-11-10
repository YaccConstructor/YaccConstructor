namespace VSYardModule
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
        [<Export>]
        [<Name("yard")>]
        [<BaseDefinition("code")>]
        let mutable ookContentType : ContentTypeDefinition = null
        member self.OokContentType 
            with get () = ookContentType
            and set oot = ookContentType <- oot

        [<Export>]
        [<FileExtension(".yrd")>]
        [<ContentType("yard")>]
        let mutable ookFileType : FileExtensionToContentTypeDefinition = null
        member self.OokFileType 
            with get () = ookContentType
            and set oft = ookContentType <- oft

        [<Import>]
        let mutable classificationTypeRegistry : IClassificationTypeRegistryService = null
        member self.ClassificationTypeRegistry
            with get () = classificationTypeRegistry
            and set ctr = classificationTypeRegistry <- ctr

        [<Import>]
        let mutable aggregatorFactory : IBufferTagAggregatorFactoryService = null
        member self.AggregatorFactory 
            with get () = aggregatorFactory
            and set af = aggregatorFactory <- af

        interface ITaggerProvider with
            member this. CreateTagger<'T when 'T :> ITag>(buffer : ITextBuffer) : ITagger<'T> =
                let ookTagAggregator : ITagAggregator<TokenTag> = aggregatorFactory.CreateTagAggregator<TokenTag>(buffer)
                MyClassifier(buffer, ookTagAggregator, ClassificationTypeRegistry) :> ITagger<'T>; 
    end
type internal MyClassifier () = class
    interface ITagger<ClassificationTag>
    end