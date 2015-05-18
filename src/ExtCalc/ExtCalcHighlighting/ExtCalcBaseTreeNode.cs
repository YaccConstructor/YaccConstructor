using System.Collections.Generic;
using System.Text;
using System.Linq;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Impl;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Text;
using JetBrains.Util;

namespace ExtCalcHighlighting
{
    public class ExtCalcBaseTreeNode : ITreeNode
    {
        private NodeUserDataHolder dataHolder = new NodeUserDataHolder();
        public ITreeNode Parent
        {
            get { return PersistentUserData.GetData(Constants.Parent); }
        }

        public ITreeNode FirstChild
        {
            get { return PersistentUserData.GetData(Constants.FirstChild); }
        }

        public ITreeNode LastChild
        {
            get { return PersistentUserData.GetData(Constants.LastChild); }
        }

        public ITreeNode NextSibling
        {
            get { return PersistentUserData.GetData(Constants.NextSibling); }
        }

        public ITreeNode PrevSibling
        {
            get { return PersistentUserData.GetData(Constants.PrevSibling); }
        }

        public NodeType NodeType
        {
            get { return PersistentUserData.GetData(Constants.NodeType); }
        }

        public PsiLanguageType Language
        {
            get { return PersistentUserData.GetData(Constants.Language) ?? UnknownLanguage.Instance; }
        }

        public NodeUserData UserData { get; private set; }
        public NodeUserData PersistentUserData { get; private set; }

        public ExtCalcBaseTreeNode (string ycTokName, int ycTokNumber)
        {
            UserData = dataHolder.GetNodeUserData(this);
            PersistentUserData = dataHolder.GetNodePersistentUserData(this);

            UserData.PutData(Constants.YcTokenName, ycTokName);
            UserData.PutData(Constants.YcTokNumber, ycTokNumber.ToString());
            UserData.PutData(Constants.YcLanguage, "extcalc");
        }

        public ExtCalcBaseTreeNode (string ycTokName, int ycTokNumber, IEnumerable<DocumentRange> positions) : this (ycTokName, ycTokNumber)
        {
            var ranges = positions.ToList();
            if (ranges.Count > 0)
            {
                UserData.PutData(Constants.Document, ranges[0].Document);
                UserData.PutData(Constants.Ranges, ranges);
            }
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
            List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);
            if (ranges == null || ranges.Count == 0)
                return default(DocumentRange);

            if (curRange >= ranges.Count)
                curRange = 0;
            return ranges[curRange++];
        }

        public TreeOffset GetTreeStartOffset()
        {
            List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);
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
            List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);
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
            IDocument doc = UserData.GetData(Constants.Document);
            var needRange = new DocumentRange(doc, GetTextRange(treeTextRange));
            List<DocumentRange> ranges = UserData.GetData(Constants.Ranges);

            bool exists = ranges.Exists(range => range.Contains(needRange));

            if (!exists)
                return null;

            if (FirstChild == null)
                return this;

            for (ITreeNode child = this.FirstChild; child != null; child = child.NextSibling)
            {
                ITreeNode node = child.FindNodeAt(treeTextRange);
                if (node != null)
                    return node;
            }

            return null;
        }

        public ICollection<ITreeNode> FindNodesAt(TreeOffset treeTextOffset)
        {
            return default(ICollection<ITreeNode>);
        }

        public ITreeNode FindTokenAt(TreeOffset treeTextOffset)
        {
            return null;
        }

        private static TextRange GetTextRange(TreeTextRange treeTextRange)
        {
            return new TextRange(treeTextRange.StartOffset.Offset, treeTextRange.EndOffset.Offset);
        }
    }
}

