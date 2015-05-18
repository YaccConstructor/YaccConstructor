using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class RNGLR_EOFTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "rnglr_eof";
        private static int ycTokNumber = 27;

        public RNGLR_EOFTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
            Helper.YcHelper.AddYcItem(ycTokName, ycTokNumber, "extcalc");
        }

        public RNGLR_EOFTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

