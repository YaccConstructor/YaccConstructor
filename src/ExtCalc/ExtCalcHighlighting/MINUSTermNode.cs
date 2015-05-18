using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class MINUSTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "minus";
        private static int ycTokNumber = 22;

        public MINUSTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public MINUSTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

