using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class Yard_some_1NonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "yard_some_1";
        private static int ycTokNumber = 16;

        public Yard_some_1NonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public Yard_some_1NonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

