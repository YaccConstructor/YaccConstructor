using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class AssignNonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "assign";
        private static int ycTokNumber = 0;

        public AssignNonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public AssignNonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

