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

namespace CalcHighlighting
{
    public class Highlight_MINUSNonTermNode : IAbstractTreeNode
    {
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
        private List<DocumentRange> ranges = new List<DocumentRange>();
        private string text;
        public Highlight_MINUSNonTermNode (string s)
        {
            text = s;
        }

        public Highlight_MINUSNonTermNode (string s, object positions)
        {
            text = s;
            SetPositions(positions as IEnumerable<DocumentRange>);
        }

        public void SetPositions(object obj)
        {
            var positions = obj as IEnumerable<DocumentRange>;
            if (positions != null)
            {
                ranges = positions.ToList();
                documentRange = ranges[0];
            }
        }
        public void SetDocumentRange(DocumentRange range)
        {
            documentRange = range;
        }
        public void DocumentRangeSetStartTo(int start)
        {
            documentRange = documentRange.SetStartTo(start);
        }
        public void DocumentRangeSetEndTo(int end)
        {
            documentRange = documentRange.SetEndTo(end);
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
        public DocumentRange GetNavigationRange()
        {
            return documentRange;
        }
        public TreeOffset GetTreeStartOffset()
        {
            return new TreeOffset();
        }
        public int GetTextLength()
        {
            return text.Length;
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
            return new StringBuffer(text);
        }
        public string GetText()
        {
            return text;
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
        public void SetParent(ITreeNode parent)
        {
            Parent = parent;
        }
        public void SetFirstChild(ITreeNode firstChild)
        {
            FirstChild = firstChild;
        }
        public void SetLastChild(ITreeNode lastChild)
        {
            LastChild = lastChild;
        }
        public void SetNextSibling(ITreeNode nextSibling)
        {
            NextSibling = nextSibling;
        }
        public void SetPrevSibling(ITreeNode prevSibling)
        {
            PrevSibling = prevSibling;
        }
        public void SetPsiLanguageType(PsiLanguageType languageType)
        {
            Language = languageType;
        }
        public void SetNodeUserData(NodeUserData userData)
        {
            UserData = userData;
        }
        public void SetPersistentUserData(NodeUserData persistentUserData)
        {
            PersistentUserData = persistentUserData;
        }
        public void Accept(TreeNodeVisitor visitor)
        {
            visitor.VisitNode(this);
        }
        public void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context)
        {
            visitor.VisitSomething(this, context);
        }
        public TResult Accept<TContext, TResult>(TreeNodeVisitor<TContext, TResult> visitor, TContext context)
        {
            visitor.VisitSomething(this, context);
            return default(TResult);
        }
    }
}

