namespace VSYard.BraceMatching
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.Composition;
    using Microsoft.VisualStudio.Text;
    using Microsoft.VisualStudio.Text.Classification;
    using Microsoft.VisualStudio.Text.Editor;
    using Microsoft.VisualStudio.Text.Tagging;
    using Microsoft.VisualStudio.Utilities;
    using System.Linq;
    using System.Windows.Media;

    [Export(typeof(EditorFormatDefinition))]
    [Name("green")]
    [UserVisible(true)]
    internal class HighlightFormatDefinition1 : MarkerFormatDefinition
    {
        public HighlightFormatDefinition1()
        {
            this.BackgroundColor = Colors.Aquamarine;
            this.ForegroundColor = Colors.Teal;
            this.DisplayName = "green element!";
            this.ZOrder = 5;
        }
    }


    [Export(typeof(IViewTaggerProvider))]
    [ContentType("yardtype")]
    [TagType(typeof(TextMarkerTag))]
    internal class BraceMatchingTaggerProvider : IViewTaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            if (textView == null)
                return null;

            //provide highlighting only on the top-level buffer
            if (textView.TextBuffer != buffer)
                return null;

            return new VSYardNS.BraceMatchingTagger(textView, buffer) as ITagger<T>;
        }
    }
}
