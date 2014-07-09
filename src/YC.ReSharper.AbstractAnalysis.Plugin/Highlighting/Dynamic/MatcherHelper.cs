using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;
using JetBrains.ReSharper.Feature.Services.CSharp;
using JetBrains.ReSharper.Psi.Tree;
using YC.ReSharper.AbstractAnalysis.Plugin.Core;

public static class MatcherHelper
{
    public static Processor YcProcessor { get; set; }
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