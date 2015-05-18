using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class StartNonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "start";
        private static int ycTokNumber = 14;

        public StartNonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public StartNonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

