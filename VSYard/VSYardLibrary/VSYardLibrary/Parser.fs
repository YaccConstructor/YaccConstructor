namespace VSYardNS

open System
open System.Collections.Generic
open System.Linq
open System.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open System.ComponentModel.Composition

(*[<Export(typeof<ITaggerProvider>)>]
[<TagType(typeof<IOutliningRegionTag>)>]
[<ContentType("yard")>]
type internal Parser () = class
    interface ITaggerProvider with
        member this.CreateTagger<'T when 'T :> ITag>(buffer : ITextBuffer ) : ITagger<'T>  = 
            buffer.Properties.GetOrCreateSingletonProperty<ITagger<'T>>(fun () -> null (*new OutliningTagger(buffer) :> ITagger<'T>*))
end*)