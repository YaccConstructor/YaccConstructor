using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Gen
{
    public abstract class TreeNodeVisitor
    {
        public virtual void VisitNode([JetBrains.Annotations.NotNull] ITreeNode node)
        {
        }
        
        public virtual void VisitVariableName([JetBrains.Annotations.NotNull] ITreeNode node)
        {
            VisitNode(node);
        }
    }
}
