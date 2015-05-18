using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class ErrorNonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "error";
        private static int ycTokNumber = 1;

        public ErrorNonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public ErrorNonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

