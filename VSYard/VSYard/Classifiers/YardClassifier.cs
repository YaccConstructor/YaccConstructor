using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;


namespace MyCompany.VSYard.Classifiers
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("yard")]
    [TagType(typeof(ClassificationTag))]
    internal sealed class YardClassifierProvider : ITaggerProvider
    {
        [Export]
        [Name("yard")]
        [BaseDefinition("code")]
        internal static ContentTypeDefinition OokContentType = null;

        [Export]
        [FileExtension(".yrd")]
        [ContentType("yard")]
        internal static FileExtensionToContentTypeDefinition OokFileType = null;

        [Import]
        internal IClassificationTypeRegistryService ClassificationTypeRegistry = null;

        [Import]
        internal IBufferTagAggregatorFactoryService aggregatorFactory = null;

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {

            ITagAggregator<VSYardNS.TokenTag> ookTagAggregator =
                                            aggregatorFactory.CreateTagAggregator<VSYardNS.TokenTag>(buffer);

            return new VSYardNS.YardClassifier(buffer, ookTagAggregator, ClassificationTypeRegistry) as ITagger<T>;
        }

    }
    /*internal sealed class YardClassifier : ITagger<ClassificationTag>
    {
        ITextBuffer _buffer;
        ITagAggregator<VSYardNS.TokenTag> _aggregator;
        IDictionary<VSYardNS.TokenTypes, IClassificationType> _ookTypes;

        internal YardClassifier(ITextBuffer buffer,
                               ITagAggregator<VSYardNS.TokenTag> ookTagAggregator,
                               IClassificationTypeRegistryService typeService)
        {
            _buffer = buffer;
            _aggregator = ookTagAggregator;
            _ookTypes = new Dictionary<VSYardNS.TokenTypes, IClassificationType>();
            _ookTypes[VSYardNS.TokenTypes.TLitersl] = typeService.GetClassificationType("ook!");
            _ookTypes[VSYardNS.TokenTypes.TNonterm] = typeService.GetClassificationType("ook.");
            _ookTypes[VSYardNS.TokenTypes.TTerm] = typeService.GetClassificationType("ook?");
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged
        {
            add { }
            remove { }
        }

        public IEnumerable<ITagSpan<ClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {

            foreach (var tagSpan in this._aggregator.GetTags(spans))
            {
                var tagSpans = tagSpan.Span.GetSpans(spans[0].Snapshot);
                yield return
                    new TagSpan<ClassificationTag>(tagSpans[0],
                                                   new ClassificationTag(_ookTypes[tagSpan.Tag.Type]));
            }
        }
    }*/

}