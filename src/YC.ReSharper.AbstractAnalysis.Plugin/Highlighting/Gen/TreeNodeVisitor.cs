using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Gen
{
    public abstract class TreeNodeVisitor<TContext>
    {
        public virtual void VisitNode([JetBrains.Annotations.NotNull] JetBrains.ReSharper.Psi.Tree.ITreeNode node, TContext context)
        {
        }
        
        public virtual void VisitSomething([JetBrains.Annotations.NotNull] ITreeNode node, TContext context)
        {
            VisitNode(node, context);
        }
    }
}
