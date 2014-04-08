using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Core
{
    public interface IAbstractTreeNode : ITreeNode
    {
        //bool IsProcessed { get; set; }
        //bool IsVisited { get; set; }
        void Accept(TreeNodeVisitor visitor);
        void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context);
        TResult Accept<TContext, TResult>(TreeNodeVisitor<TContext, TResult> visitor, TContext context);

        void SetNextSibling(ITreeNode sibling);
        void SetPrevSibling(ITreeNode sibling);
        void SetParent(ITreeNode parent);
        void SetFirstChild(ITreeNode child);
        void SetLastChild(ITreeNode child);
    }
}