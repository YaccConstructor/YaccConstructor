using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class LBRACETermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "lbrace";
        private static int ycTokNumber = 21;

        public LBRACETermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public LBRACETermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

