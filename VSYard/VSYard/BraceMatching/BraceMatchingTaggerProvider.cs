namespace YC.VSYard.BraceMatching
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
    using EnvDTE;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.Shell.Interop;    

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
            //It is exampe of getting root *.yrd file of active project.
            //Should be removed
            //var t = YC.VSYard.Helpers.SolutionNavigatorHelper.GetRootYrd
            //        (YC.VSYard.Helpers.SolutionNavigatorHelper.GetActiveProject());

            if (textView == null
               || textView.TextBuffer != buffer) //provide highlighting only on the top-level buffer
               return null;

            return new VSYardNS.BraceMatchingTagger(textView, buffer) as ITagger<T>;
        }
    }
}
