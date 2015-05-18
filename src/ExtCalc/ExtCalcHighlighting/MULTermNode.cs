using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class MULTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "mul";
        private static int ycTokNumber = 23;

        public MULTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public MULTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

