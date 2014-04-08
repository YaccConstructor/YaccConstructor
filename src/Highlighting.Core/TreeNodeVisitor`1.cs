using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Core
{
    public abstract class TreeNodeVisitor
    {
        public virtual void VisitNode([JetBrains.Annotations.NotNull] ITreeNode node)
        {
        }
        
        public virtual void VisitSomething([JetBrains.Annotations.NotNull] ITreeNode node)
        {
            VisitNode(node);
        }
    }
}
