using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class ExprNonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "expr";
        private static int ycTokNumber = 2;

        public ExprNonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public ExprNonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

