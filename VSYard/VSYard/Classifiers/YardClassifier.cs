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
    internal sealed class YardClassifier : ITaggerProvider
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

            ITagAggregator<TokenTag> ookTagAggregator =
                                            aggregatorFactory.CreateTagAggregator<TokenTag>(buffer);

            return new MyClassifier(buffer, ookTagAggregator, ClassificationTypeRegistry) as ITagger<T>;
        }

    }
    internal sealed class MyClassifier : ITagger<ClassificationTag>
    {
        ITextBuffer _buffer;
        ITagAggregator<TokenTag> _aggregator;
        IDictionary<TokenTypes, IClassificationType> _ookTypes;

        internal MyClassifier(ITextBuffer buffer,
                               ITagAggregator<TokenTag> ookTagAggregator,
                               IClassificationTypeRegistryService typeService)
        {
            _buffer = buffer;
            _aggregator = ookTagAggregator;
            _ookTypes = new Dictionary<TokenTypes, IClassificationType>();
            _ookTypes[TokenTypes.OokExclaimation] = typeService.GetClassificationType("ook!");
            _ookTypes[TokenTypes.OokPeriod] = typeService.GetClassificationType("ook.");
            _ookTypes[TokenTypes.OokQuestion] = typeService.GetClassificationType("ook?");
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
                                                   new ClassificationTag(_ookTypes[tagSpan.Tag.type]));
            }
        }
    }

}
