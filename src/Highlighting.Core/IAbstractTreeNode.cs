using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Core
{
    public interface IAbstractTreeNode : ITreeNode
    {
        //int Parts { get; }

        void Accept(TreeNodeVisitor visitor);
        void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context);
        TResult Accept<TContext, TResult>(TreeNodeVisitor<TContext, TResult> visitor, TContext context);

        //DocumentRange[] GetAllPositions();

        void SetNextSibling(ITreeNode sibling);
        void SetPrevSibling(ITreeNode sibling);
        void SetParent(ITreeNode parent);
        void SetFirstChild(ITreeNode child);
        void SetLastChild(ITreeNode child);
    }
}