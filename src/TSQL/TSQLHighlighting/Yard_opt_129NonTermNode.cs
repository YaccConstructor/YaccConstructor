using System.Collections.Generic;
using System.Text;
using System.Linq;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.ReSharper.Psi.Impl;
using JetBrains.Text;
using Highlighting.Core;

namespace TSQLHighlighting
{
    public class Yard_opt_129NonTermNode : ITreeNode
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
            get { return PersistentUserData.GetData(PropertyConstant.Language); }
        }

        public NodeUserData UserData { get; private set; }
        public NodeUserData PersistentUserData { get; private set; }

        public Yard_opt_129NonTermNode (string text)
        {
            UserData = DataHelper.GetNodeUserData(this);
            PersistentUserData = DataHelper.GetNodePersistentUserData(this);
            UserData.PutData(KeyConstant.Text, text);
        }

        public Yard_opt_129NonTermNode (string text, object positions) : this (text)
        {
            SetPositions(positions as IEnumerable<DocumentRange>);
        }

        public void SetPositions(IEnumerable<DocumentRange> positions)
        {
            if (positions != null)
            {
                var ranges = positions.ToList();
                UserData.PutData(KeyConstant.Ranges, ranges);
            }
        }

        public IPsiServices GetPsiServices()
        {
            return default(IPsiServices);
        }

        public IPsiModule GetPsiModule()
        {
            return this.Parent.GetPsiModule();
        }

        public IPsiSourceFile GetSourceFile()
        {
            return Parent.GetSourceFile();
        }

        public ReferenceCollection GetFirstClassReferences()
        {
            return default(ReferenceCollection);
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
            return true;
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
            return new TreeOffset();
        }

        public int GetTextLength()
        {
            string text = UserData.GetData(KeyConstant.Text);
            return text != null ? text.Length : 0;
        }

        public StringBuilder GetText(StringBuilder to)
        {
            for (ITreeNode nextSibling = this.FirstChild; nextSibling != null; nextSibling = nextSibling.NextSibling)
            {
                nextSibling.GetText(to);
            }
            return to;
        }

        public IBuffer GetTextAsBuffer()
        {
            var text = UserData.GetData(KeyConstant.Text);
            return new StringBuffer(text?? "");
        }

        public string GetText()
        {
            string text = UserData.GetData(KeyConstant.Text);
            return text?? "";
        }

        public ITreeNode FindNodeAt(TreeTextRange treeTextRange)
        {
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

    }
}

