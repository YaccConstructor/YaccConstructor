using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;

namespace Highlighting.Core
{
      public abstract class MyFileElement : FileElementBase
      {
        public virtual void Accept(TreeNodeVisitor visitor)
        {
          visitor.VisitNode(this);
        }

        public virtual void Accept<TContext>(TreeNodeVisitor<TContext> visitor, TContext context)
        {
          visitor.VisitNode(this, context);
        }

        public virtual TReturn Accept<TContext, TReturn>(TreeNodeVisitor<TContext, TReturn> visitor, TContext context)
        {
          return visitor.VisitNode(this, context);
        }
      }
}
