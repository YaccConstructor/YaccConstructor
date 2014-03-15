using System.Collections.Generic;
using System.Text;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Text;

namespace Highlighting.Core
{
    public class AbstractTreeNode : IAbstractTreeNode
    {
        public Dictionary<ITreeNode, ITreeNode> ParentAndPrevSibling { get; private set; }
        public Dictionary<ITreeNode, ITreeNode> ParentAndNextSibling { get; private set; }
        
        public ITreeNode Parent { get; private set; }
        public ITreeNode FirstChild { get; private set; }
        public ITreeNode LastChild { get; private set; }
        public ITreeNode NextSibling { get; private set; }
        public ITreeNode PrevSibling { get; private set; }
        public NodeType NodeType { get; private set; }
        public PsiLanguageType Language { get; private set; }
        public NodeUserData UserData { get; private set; }
        public NodeUserData PersistentUserData { get; private set; }
        
        private DocumentRange documentRange = new DocumentRange();
        private string text;
        public AbstractTreeNode(string s)
        {
            text = s;
            ParentAndPrevSibling = new Dictionary<ITreeNode, ITreeNode>();
            ParentAndNextSibling = new Dictionary<ITreeNode, ITreeNode>();
        }

        public virtual void SetDocument(IDocument document)
        {
            documentRange = new DocumentRange(document, 0);
        }

        public virtual void SetDocumentRange(DocumentRange range)
        {
            documentRange = range;
        }

        public virtual void DocumentRangeSetStartTo(int start)
        {
            documentRange = documentRange.SetStartTo(start);
        }

        public virtual void DocumentRangeSetEndTo(int end)
        {
            documentRange = documentRange.SetEndTo(end);
        }

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
            return documentRange;
        }

        public virtual TreeOffset GetTreeStartOffset()
        {
            return new TreeOffset();
        }

        public virtual int GetTextLength()
        {
            return text.Length;
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
            return new StringBuffer(text);
        }

        public virtual string GetText()
        {
            //StringBuilder to = (this.MyCachedLength >= 0) ? new StringBuilder(this.myCachedLength) : new StringBuilder();
            //return this.GetText(to).ToString();
            return text;
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

        public virtual void SetParent(ITreeNode parent)
        {
            if (!ParentAndPrevSibling.ContainsKey(parent))
            {
                ParentAndPrevSibling.Add(parent, null);
                ParentAndNextSibling.Add(parent, null);
            }

            Parent = parent;
            PrevSibling = ParentAndPrevSibling[Parent];
            NextSibling = ParentAndNextSibling[Parent];
        }

        public virtual void SetFirstChild(ITreeNode firstChild)
        {
            FirstChild = firstChild;
        }

        public virtual void SetLastChild(ITreeNode lastChild)
        {
            LastChild = lastChild;
        }

        public virtual void SetNextSibling(ITreeNode nextSibling)
        {
            ParentAndNextSibling[Parent] = nextSibling;
            NextSibling = nextSibling;
        }

        public virtual void SetPrevSibling(ITreeNode prevSibling)
        {
            ParentAndPrevSibling[Parent] = prevSibling;
            PrevSibling = prevSibling;
        }

        public virtual void SetPsiLanguageType(PsiLanguageType languageType)
        {
            Language = languageType;
        }

        public virtual void SetNodeUserData(NodeUserData userData)
        {
            UserData = userData;
        }

        public virtual void SetPersistentUserData(NodeUserData persistentUserData)
        {
            PersistentUserData = persistentUserData;
        }

        public virtual void Accept(TreeNodeVisitor visitor)
        {
            visitor.VisitNode(this);
        }

        public virtual void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context)
        {
            visitor.VisitSomething(this, context);
        }

        public virtual TResult Accept<TContext, TResult>(TreeNodeVisitor<TContext, TResult> visitor, TContext context)
        {
            visitor.VisitSomething(this, context);
            return default(TResult);
        }
    }
}
