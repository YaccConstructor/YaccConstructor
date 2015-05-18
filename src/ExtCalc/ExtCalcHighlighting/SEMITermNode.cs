using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class SEMITermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "semi";
        private static int ycTokNumber = 28;

        public SEMITermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public SEMITermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

