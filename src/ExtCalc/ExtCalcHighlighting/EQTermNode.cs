using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class EQTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "eq";
        private static int ycTokNumber = 19;

        public EQTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public EQTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

