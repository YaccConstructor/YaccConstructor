using System.Collections.Generic;
using System.Text;
using System.Linq;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Text;
using Highlighting.Core;

namespace TSQLHighlighting
{
    public class Yard_exp_brackets_198NonTermNode : ITreeNode
    {
        public ITreeNode Parent
        {
            get { return PersistentUserData.GetData(PropertyConstant.Parent); }
        }

        public ITreeNode FirstChild
        {
            get { return PersistentUserData.GetData(PropertyConstant.FirstChild); }
        }

        public ITreeNode LastChild
        {
            get { return PersistentUserData.GetData(PropertyConstant.LastChild); }
        }

        public ITreeNode NextSibling
        {
            get { return PersistentUserData.GetData(PropertyConstant.NextSibling); }
        }

        public ITreeNode PrevSibling
        {
            get { return PersistentUserData.GetData(PropertyConstant.PrevSibling); }
        }

        public NodeType NodeType
        {
            get { return PersistentUserData.GetData(PropertyConstant.NodeType); }
        }

        public PsiLanguageType Language
        {
            get { return PersistentUserData.GetData(PropertyConstant.Language) ?? UnknownLanguage.Instance; }
        }

        public NodeUserData UserData { get; private set; }
        public NodeUserData PersistentUserData { get; private set; }

        public Yard_exp_brackets_198NonTermNode (string ycTokName) : this (ycTokName, string.Empty)
        {
        }

        public Yard_exp_brackets_198NonTermNode (string ycTokName, string ycValue)
        {
            UserData = DataHelper.GetNodeUserData(this);
            PersistentUserData = DataHelper.GetNodePersistentUserData(this);

            UserData.PutData(KeyConstant.YcTokName, ycTokName);
            UserData.PutData(KeyConstant.YcValue, ycValue);

            YcHelper.AddYcItem(ycTokName, ycValue);
        }

        public Yard_exp_brackets_198NonTermNode (string ycTokName, string ycValue, object positions) : this (ycTokName, ycValue)
        {
            SetPositions(positions as IEnumerable<DocumentRange>);
        }

        private void SetPositions(IEnumerable<DocumentRange> positions)
        {
            if (positions == null)
                return;

            var ranges = positions.ToList();
            UserData.PutData(KeyConstant.Ranges, ranges);
        }

        public IPsiServices GetPsiServices()
        {
            return default(IPsiServices);
        }

        public IPsiModule GetPsiModule()
        {
            return default(IPsiModule);
        }

        public IPsiSourceFile GetSourceFile()
        {
            return default(IPsiSourceFile);
        }

        public ReferenceCollection GetFirstClassReferences()
        {
            return ReferenceCollection.Empty;
        }

        public void ProcessDescendantsForResolve(IRecursiveElementProcessor processor)
        {
            return;
        }

        public T GetContainingNode<T>(bool returnThis = false) where T : ITreeNode
        {
            return default(T);
        }

        public bool Contains(ITreeNode other)
        {
            if (this.FirstChild != null)
                return this.Children().Contains(other);
            else
                return this == other;
        }

        public bool IsPhysical()
        {
            return true;
        }

        public bool IsValid()
        {
            return true;
        }

        public bool IsStub()
        {
            return false;
        }

        public bool IsFiltered()
        {
            return true;
        }

        private int curRange = 0;
        //Calls by external code
        public DocumentRange GetNavigationRange()
        {
            List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);
            if (ranges == null || ranges.Count == 0)
                return default(DocumentRange);

            if (curRange >= ranges.Count)
                curRange = 0;
            return ranges[curRange++];
        }

        public TreeOffset GetTreeStartOffset()
        {
            List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);
            if (ranges == null || ranges.Count == 0)
                return TreeOffset.InvalidOffset;

            return new TreeOffset(ranges[0].TextRange.StartOffset);
        }

        public int GetTextLength()
        {
            return GetText(new StringBuilder()).Length;
        }

        public StringBuilder GetText(StringBuilder to)
        {
            List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);
            foreach (DocumentRange range in ranges)
            {
                to.Append(range.GetText());
            }
            return to;
        }

        public IBuffer GetTextAsBuffer()
        {
            return new StringBuffer(GetText());
        }

        public string GetText()
        {
            return GetText(new StringBuilder()).ToString();
        }

        public ITreeNode FindNodeAt(TreeTextRange treeTextRange)
        {
            ITreeNode needNode = null;
            for (ITreeNode child = this.FirstChild; child != null; child = child.NextSibling)
            {
                var childOffset = child.GetTreeStartOffset();
                if (!childOffset.IsValid())
                    continue;
                if (child.GetTreeStartOffset() <= treeTextRange.StartOffset)
                    needNode = child;
                else
                    break;
            }
            //needNode = needNode.PrevSibling;

            if (needNode == null || needNode.FirstChild == null) return needNode;
            else return needNode.FindNodeAt(treeTextRange);
        }

        public ICollection<ITreeNode> FindNodesAt(TreeOffset treeTextOffset)
        {
            return default(ICollection<ITreeNode>);
        }

        public ITreeNode FindTokenAt(TreeOffset treeTextOffset)
        {
            return null;
        }

    }
}

