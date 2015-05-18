using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class RBRACETermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "rbrace";
        private static int ycTokNumber = 26;

        public RBRACETermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public RBRACETermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

