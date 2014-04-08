using System.Collections.Generic;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Inspections
{
    public static class Helper
    {
        public static ITreeNode TreeNode { get; set; }

        public static Dictionary<string, string> ColorDictionary { get; set; }
    }
}
