using System.Collections.Generic;
using JetBrains.Annotations;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting
{
    class TreeNodeProcessor : IRecursiveElementProcessor
    {
        private readonly IHighlightingConsumer myConsumer;
        private readonly IFile file;

        public TreeNodeProcessor(IHighlightingConsumer consumer, [NotNull] IFile file)
        {
            myConsumer = consumer;
            this.file = file;
        }

        #region IRecursiveElementProcessor Members

        public bool ProcessingIsFinished
        {
            get { return false; }
        }

        public void ProcessBeforeInterior(ITreeNode element)
        {
        }

        public void ProcessAfterInterior(ITreeNode element)
        {
            if (element.FirstChild == null)
                VisitLeaf(element, myConsumer);
        }

        public bool InteriorShouldBeProcessed(ITreeNode element)
        {
            return true;
        }

        #endregion

        public void VisitLeaf(ITreeNode treeNode, IHighlightingConsumer consumer)
        {
            ICollection<DocumentRange> colorConstantRange = treeNode.UserData.GetData(Constants.Ranges);

            if (colorConstantRange == null)
                return;

            colorConstantRange.ForEach(
                range =>
                AddHighLighting(range, consumer, new TokenHighlighting(treeNode)));
        }

        private void AddHighLighting(DocumentRange range, IHighlightingConsumer consumer, IHighlighting highlighting)
        {
            var info = new HighlightingInfo(range, highlighting, new Severity?());
            consumer.AddHighlighting(info.Highlighting, file);
        }
    }
}
