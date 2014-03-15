using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Gen
{
    public abstract class TreeNodeVisitor<TContext, TReturn>
    {
        public virtual TReturn VisitNode([JetBrains.Annotations.NotNull] ITreeNode node, TContext context)
        {
            return default(TReturn);
        }

        public virtual TReturn VisitSomething([JetBrains.Annotations.NotNull] ITreeNode node, TContext context)
        {
            return VisitNode(node, context);
        }
    }
}
