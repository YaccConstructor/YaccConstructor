using System.Collections.Generic;
using System.Linq;

using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting.Dynamic
{
    internal static class ExistingRanges
    {
        public static readonly Dictionary<IDocument, List<ITreeNode>> DocumentToRange = new Dictionary<IDocument, List<ITreeNode>>();

        public static void AddRanges(IEnumerable<ITreeNode> ranges)
        {
            ranges.ForEach(AddRange);
        }

        private static void AddRange(ITreeNode node)
        {
            IDocument document = node.UserData.GetData(Constants.Document);
            if (document == null)
            {
                IEnumerable<DocumentRange> ranges = node.UserData.GetData(Constants.Ranges);
                if (ranges == null)
                    return;
                document = ranges.FirstOrDefault().Document;
            }

            if (!DocumentToRange.ContainsKey(document))
                DocumentToRange[document] = new List<ITreeNode>();

            DocumentToRange[document].Add(node);
        }

        public static IEnumerable<ITreeNode> GetTreeNodes(IDocument doc)
        {
            return DocumentToRange.ContainsKey(doc) ? DocumentToRange[doc] : new List<ITreeNode>();
        }

        public static void ClearExistingRanges(IDocument document)
        {
            if (!DocumentToRange.ContainsKey(document))
                return;
            DocumentToRange[document].Clear();
        }
    }
}
