using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class VARIABLETermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "variable";
        private static int ycTokNumber = 29;

        public VARIABLETermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public VARIABLETermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

