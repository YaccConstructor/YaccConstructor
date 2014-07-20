using System.Collections.Generic;
using JetBrains.ReSharper.Psi.Tree;

public static class MatcherHelper
{
    public static YC.AbstractAnalysis.Helper.ReSharperHelper YcProcessor { get; set; }
    public static List<ITreeNode> NodeCover { get; private set; }

    static MatcherHelper()
    {
        NodeCover = new List<ITreeNode>();
    }

    public static void ClearNodeCover()
    {
        NodeCover.Clear();
    }
}