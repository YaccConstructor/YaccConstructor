using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class Yard_start_ruleNonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "yard_start_rule";
        private static int ycTokNumber = 17;

        public Yard_start_ruleNonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public Yard_start_ruleNonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

