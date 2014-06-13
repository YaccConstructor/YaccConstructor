using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Impl;
using JetBrains.ReSharper.Psi.Services.CSharp.StructuralSearch.Matchers;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Core
{
    public interface IMyTreeNode : INodeUserDataHolderOwner //INodeUserDataHolderOwner : ITreeNode
    {
        //void Accept(ITreeNodeVisitor visitor);
        //void Accept<TContext>(ITreeNodeVisitor<TContext> visitor, TContext context);
        //TResult Accept<TContext, TResult>(ITreeNodeVisitor<TContext, TResult> visitor, TContext context);

        /*void SetNextSibling(ITreeNode sibling);
        void SetPrevSibling(ITreeNode sibling);
        void SetParent(ITreeNode parent);
        void SetFirstChild(ITreeNode child);
        void SetLastChild(ITreeNode child);
         */
    }
}