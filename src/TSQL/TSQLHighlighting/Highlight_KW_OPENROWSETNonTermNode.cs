using System.Collections.Generic;
using System.Text;
using System.Linq;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Modules;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Text;
using JetBrains.Util;
using Highlighting.Core;

namespace TSQLHighlighting
{
    public class Highlight_KW_OPENROWSETNonTermNode : IAbstractTreeNode
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

        public int Parts { get; private set; }
        private List<DocumentRange> ranges = new List<DocumentRange>();
        private string text;
        private int curRange = 0;

        public Highlight_KW_OPENROWSETNonTermNode (string s)
        {
            text = s;
        }

        public Highlight_KW_OPENROWSETNonTermNode (string s, object positions)
        {
            text = s;
            SetPositions(positions as IEnumerable<DocumentRange>);
        }

        public void SetPositions(IEnumerable<DocumentRange> positions)
        {
            if (positions != null)
            {
                ranges = positions.ToList();
                //UserData.PutData(new Key<List<DocumentRange>>("ranges"), ranges);
                //Parts = ranges.Count;
            }
        }

        public DocumentRange[] GetAllPositions()
        {
            return ranges.ToArray();
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
            if (ranges.Count == 0)
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

