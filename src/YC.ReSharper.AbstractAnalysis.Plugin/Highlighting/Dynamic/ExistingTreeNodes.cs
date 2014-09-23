using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic
{
    public static class ExistingTreeNodes
    {
        public static Dictionary<IDocument, List<ITreeNode>> ExistingTrees = new Dictionary<IDocument, List<ITreeNode>>();

        public static void AddTree(ITreeNode tree)
        {
            IDocument document = tree.UserData.GetData(KeyConstant.Document);
            if (document == null)
                document = tree.UserData.GetData(KeyConstant.Ranges).FirstOrDefault().Document;

            if (!ExistingTrees.ContainsKey(document))
                ExistingTrees.Add(document, new List<ITreeNode>());

            ExistingTrees[document].Add(tree);
        }

        public static List<ITreeNode> GeTreeNodes(IDocument doc)
        {
            if (ExistingTrees.ContainsKey(doc))
                return ExistingTrees[doc];
            return new List<ITreeNode>();
        }

        public static void ClearExistingTree(IDocument document)
        {
            if (!ExistingTrees.ContainsKey(document))
                return;

            ExistingTrees[document].Clear();
        }
    }
}
