using System.Collections.Generic;
using JetBrains.DocumentModel;
using YC.SDK.ReSharper;

namespace ExtCalcHighlighting
{
    public class Yard_exp_brackets_1NonTermNode : ExtCalcBaseTreeNode
    {
        private static string ycTokName = "yard_exp_brackets_1";
        private static int ycTokNumber = 15;

        public Yard_exp_brackets_1NonTermNode (IEnumerable<DocumentRange> positions)
            : base(ycTokName, ycTokNumber, positions)
        {
        }

        public Yard_exp_brackets_1NonTermNode() : base(ycTokName, ycTokNumber)
        {
        }
    }
}

