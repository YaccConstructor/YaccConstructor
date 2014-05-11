using JetBrains.Annotations;
using JetBrains.ReSharper.Psi.Tree;

namespace Highlighting.Core
{
    public interface ITreeNodeVisitor<TContext>
    {
        void VisitLeaf([NotNull] ITreeNode node, TContext context);
    }
}
