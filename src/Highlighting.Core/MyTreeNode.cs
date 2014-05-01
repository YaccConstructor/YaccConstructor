using System.Collections.Generic;
using System.Linq;
using System.Text;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Impl;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Text;
using JetBrains.Util;

namespace Highlighting.Core
{
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
            get { return PersistentUserData.GetData(PropertyConstant.Language); }
        }

        public NodeUserData UserData { get; private set; }
        public NodeUserData PersistentUserData { get; private set; }
        //public NodeUserDataHolder NodeUserDataHolder { get; private set; }
        //public int Parts { get; private set; }

        //private DocumentRange documentRange = new DocumentRange();
        //private List<DocumentRange> ranges = new List<DocumentRange>();
        private int curInd;

        //private string text;

        public MyTreeNode(string text)
        {
            UserData = DataHelper.GetNodeUserData(this);
            PersistentUserData = DataHelper.GetNodeUserData(this);
            UserData.PutData(KeyConstant.Text, text);
        }

        public MyTreeNode(string text, object positions)
            : this(text)
        {
            SetPositions(positions);
        }

        public void SetPositions(object obj)
        {
            var positions = obj as IEnumerable<DocumentRange>;
            if (positions != null)
            {
                var ranges = positions.ToList();

                UserData.PutData(KeyConstant.Ranges, ranges);
            }
        }

        //public virtual DocumentRange[] GetAllPositions()
        //{
        //    return ranges.ToArray();
        //}

        public virtual IPsiServices GetPsiServices()
        {
            return default(IPsiServices);
        }

        public virtual IPsiModule GetPsiModule()
        {
            return Parent.GetPsiModule();
        }

        public virtual IPsiSourceFile GetSourceFile()
        {
            return Parent.GetSourceFile();
        }

        public virtual ReferenceCollection GetFirstClassReferences()
        {
            return default(ReferenceCollection);
        }

        public virtual void ProcessDescendantsForResolve(IRecursiveElementProcessor processor)
        {
            return;
        }

        public virtual T GetContainingNode<T>(bool returnThis = false) where T : ITreeNode
        {
            return default(T);
        }

        public virtual bool Contains(ITreeNode other)
        {
            return true;
        }

        public virtual bool IsPhysical()
        {
            return true;
        }

        public virtual bool IsValid()
        {
            return true;
        }

        public virtual bool IsStub()
        {
            return false;
        }

        public virtual bool IsFiltered()
        {
            return true;
        }

        public virtual DocumentRange GetNavigationRange()
        {
            var ranges = UserData.GetData(KeyConstant.Ranges);
            if (ranges.Count == 0)
                return default(DocumentRange);
            if (curInd >= ranges.Count)
                curInd = 0;
            return ranges[curInd++];
        }

        public virtual TreeOffset GetTreeStartOffset()
        {
            return new TreeOffset();
        }

        public virtual int GetTextLength()
        {
            var text = UserData.GetData(KeyConstant.Text);
            return text != null ? text.Length : 0;
        }

        public virtual StringBuilder GetText(StringBuilder to)
        {
            for (ITreeNode nextSibling = this.FirstChild; nextSibling != null; nextSibling = nextSibling.NextSibling)
            {
                nextSibling.GetText(to);
            }
            return to;
        }

        public virtual IBuffer GetTextAsBuffer()
        {
            var text = UserData.GetData(KeyConstant.Text);
            return new StringBuffer(text ?? "");
        }

        public virtual string GetText()
        {
            //StringBuilder to = (this.MyCachedLength >= 0) ? new StringBuilder(this.myCachedLength) : new StringBuilder();
            //return this.GetText(to).ToString();
            var text = UserData.GetData(KeyConstant.Text);
            return text ?? "";
        }

        public virtual ITreeNode FindNodeAt(TreeTextRange treeTextRange)
        {
            return null;
        }

        public virtual ICollection<ITreeNode> FindNodesAt(TreeOffset treeTextOffset)
        {
            return default(ICollection<ITreeNode>);
        }

        public virtual ITreeNode FindTokenAt(TreeOffset treeTextOffset)
        {
            return null;
        }


        //public DocumentRange[] GetAllPositions()
        //{
        //    return ranges.ToArray();
        //}

        //public virtual void SetNextSibling(ITreeNode nextSibling)
        //{
        //    NextSibling = nextSibling;
        //}

        //public virtual void SetPrevSibling(ITreeNode prevSibling)
        //{
        //    PrevSibling = prevSibling;
        //}

        //public virtual void SetPsiLanguageType(PsiLanguageType languageType)
        //{
        //    Language = languageType;
        //}

        //public virtual void SetNodeUserData(NodeUserData userData)
        //{
        //    UserData = userData;
        //}

        //public virtual void SetPersistentUserData(NodeUserData persistentUserData)
        //{
        //    PersistentUserData = persistentUserData;
        //}

        //public virtual void Accept(ITreeNodeVisitor visitor)
        //{
        //    visitor.VisitSomething(this);
        //}

        public virtual void Accept<TContext>(ITreeNodeVisitor<TContext> visitor, TContext context)
        {
            visitor.VisitSomething(this, context);
        }

        //public virtual TResult Accept<TContext, TResult>(ITreeNodeVisitor<TContext, TResult> visitor, TContext context)
        //{
        //    visitor.VisitSomething(this, context);
        //    return default(TResult);
        //}
    }
}
