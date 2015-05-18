using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class DIVTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "div";
        private static int ycTokNumber = 18;

        public DIVTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public DIVTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

