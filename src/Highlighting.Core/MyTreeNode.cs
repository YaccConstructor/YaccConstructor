using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Text;

namespace Highlighting.Core
{
    [Obsolete("Do not use this class. It is for experiments only.")]
    public class MyTreeNode : ITreeNode
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

        private int curInd;
        public MyTreeNode(string ycTokName)
        {
            UserData = DataHelper.GetNodeUserData(this);
            PersistentUserData = DataHelper.GetNodeUserData(this);
            UserData.PutData(KeyConstant.YcTokenName, ycTokName);
            UserData.PutData(KeyConstant.YcLanguage, "Calc");
            
            YcHelper.AddYcItem(ycTokName, -2, "Calc");
        }

        public MyTreeNode(string ycName, IEnumerable<DocumentRange> positions)
            : this(ycName)
        {
            var ranges = positions.ToList();
            if (ranges.Count > 0)
            {
                var document = ranges[0].Document;

                UserData.PutData(KeyConstant.Document, document);
                var doc2 = UserData.GetData(KeyConstant.Document);
                UserData.PutData(KeyConstant.Ranges, ranges);
            }
            var doc3 = UserData.GetData(KeyConstant.Document);
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
            return Parent.GetSourceFile();
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
            if (FirstChild != null)
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

        public DocumentRange GetNavigationRange()
        {
            var ranges = UserData.GetData(KeyConstant.Ranges);
            if (ranges == null || ranges.Count == 0)
                return default(DocumentRange);
            
            if (curInd >= ranges.Count)
                curInd = 0;
            return ranges[curInd++];
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

        //TODO: rewrite implementation
        public IBuffer GetTextAsBuffer()
        {
            return new StringBuffer(GetText());
        }

        public string GetText()
        {
            //StringBuilder to = (this.MyCachedLength >= 0) ? new StringBuilder(this.myCachedLength) : new StringBuilder();
            //return this.GetText(to).ToString();
            return GetText(new StringBuilder()).ToString();
        }

        // TODO: Normal implementation
        public ITreeNode FindNodeAt(TreeTextRange treeTextRange)
        {
            if (UserData.GetData(KeyConstant.Document) == null)
                return null;
            
            var needRange = new DocumentRange(UserData.GetData(KeyConstant.Document), treeTextRange.GetTextRange());
            

            List<DocumentRange> ranges = UserData.GetData(KeyConstant.Ranges);
            bool exists = ranges.Exists(range => range.Contains(needRange));

            if (!exists)
                return null;
            
            //FirstChild null. It means that it node is leaf. 
            if (exists && FirstChild == null)
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
    }
}
