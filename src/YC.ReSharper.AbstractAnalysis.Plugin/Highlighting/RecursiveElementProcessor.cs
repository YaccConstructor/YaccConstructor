using System.Collections.Generic;
using Highlighting.Core;
using JetBrains.Annotations;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    class RecursiveElementProcessor
    {
        private readonly IHighlightingConsumer myConsumer;
        private readonly IFile file;

        public RecursiveElementProcessor([NotNull] IHighlightingConsumer consumer, IFile file)
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

        private readonly List<TextRange> addedRanges = new List<TextRange>();
        public void VisitLeaf(ITreeNode treeNode, IHighlightingConsumer consumer)
        {
            ICollection<DocumentRange> colorConstantRange = treeNode.UserData.GetData(KeyConstant.Ranges);

            if (colorConstantRange == null)
                return;

            foreach (DocumentRange range in colorConstantRange)
            {
                if (range.Document == null || addedRanges.Contains(range.TextRange))
                    continue;

                AddHighLighting(range, consumer, new Highlighting(treeNode));
            }
        }

        private void AddHighLighting(DocumentRange range, IHighlightingConsumer consumer, IHighlighting highlighting)
        {
            var info = new HighlightingInfo(range, highlighting, new Severity?());
            if (file != null)
            {
                consumer.AddHighlighting(info.Highlighting, file);
                addedRanges.Add(range.TextRange);
            }
        }
    }
}
