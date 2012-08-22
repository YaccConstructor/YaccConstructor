//using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
//using System.ComponentModel.Composition;
//using Microsoft.VisualStudio.Text;
//using Microsoft.VisualStudio.Text.Classification;
//using Microsoft.VisualStudio.Text.Editor;
//using Microsoft.VisualStudio.Text.Tagging;
//using Microsoft.VisualStudio.Utilities;


//namespace MyCompany.VSYard.Classifiers
//{
//    [Export(typeof(ITaggerProvider))]
//    [ContentType("yard")]
//    [TagType(typeof(ClassificationTag))]
//    internal sealed class YardClassifierProvider : ITaggerProvider
//    {
//        [Export]
//        [Name("yard")]
//        [BaseDefinition("code")]
//        internal static ContentTypeDefinition OokContentType = null;

//        [Export]
//        [FileExtension(".yrd")]
//        [ContentType("yard")]
//        internal static FileExtensionToContentTypeDefinition OokFileType = null;

//        [Import]
//        internal IClassificationTypeRegistryService ClassificationTypeRegistry = null;

//        [Import]
//        internal IBufferTagAggregatorFactoryService aggregatorFactory = null;

//        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
//        {
//            ITagAggregator<VSYardNS.TokenTag> ookTagAggregator =
//                                            aggregatorFactory.CreateTagAggregator<VSYardNS.TokenTag>(buffer);

//            return new VSYardNS.YardClassifier(buffer, ookTagAggregator, ClassificationTypeRegistry) as ITagger<T>;
//        }
//    }
//}