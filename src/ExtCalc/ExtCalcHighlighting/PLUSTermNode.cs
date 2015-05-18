using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class PLUSTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "plus";
        private static int ycTokNumber = 25;

        public PLUSTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public PLUSTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

