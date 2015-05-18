using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class ERRORTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "error";
        private static int ycTokNumber = 20;

        public ERRORTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public ERRORTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

