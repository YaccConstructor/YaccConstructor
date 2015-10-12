using System.Collections.Generic;
using System.Linq;

using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;

using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting.Dynamic
{
    public static class ExistingTreeNodes
    {
        public static Dictionary<IDocument, List<ITreeNode>> ExistingTrees = new Dictionary<IDocument, List<ITreeNode>>();

        public static void AddTree(ITreeNode tree)
        {
            IDocument document = tree.UserData.GetData(Constants.Document);
            if (document == null)
                document = tree.UserData.GetData(Constants.Ranges).FirstOrDefault().Document;

            if (!ExistingTrees.ContainsKey(document))
                ExistingTrees.Add(document, new List<ITreeNode>());

            ExistingTrees[document].Add(tree);
        }

        public static List<ITreeNode> GetTreeNodes(IDocument doc)
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
