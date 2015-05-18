using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class NUMBERTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "number";
        private static int ycTokNumber = 24;

        public NUMBERTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public NUMBERTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

