using JetBrains.ReSharper.Psi;
using JetBrains.Util;

namespace Highlighting.Core
{
    public static class TreeTextRangeExtension
    {
        public static TextRange GetTextRange(this TreeTextRange treeTextRange)
        {
            return new TextRange(treeTextRange.StartOffset.Offset, treeTextRange.EndOffset.Offset);
        }
    }

    public static class TextRangeExtension
    {
        public static TreeTextRange GetTreeTextRange(this TextRange textRange)
        {
            return new TreeTextRange(new TreeOffset(textRange.StartOffset), new TreeOffset(textRange.EndOffset));
        }
    }
}
