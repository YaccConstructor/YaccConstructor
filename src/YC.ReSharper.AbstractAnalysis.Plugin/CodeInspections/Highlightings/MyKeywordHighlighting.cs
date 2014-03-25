using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Keyword", null, HighlightingGroupIds.LanguageUsage, "Keyword", @"
          Keyword", Severity.INFO, false, Internal = false)]
namespace Highlighting.Psi.CodeInspections.Highlightings
{
    [ConfigurableSeverityHighlighting("Keyword", "CSharp", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Keyword")]
    internal class MyKeywordHighlighting : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private const string AtributeId = HighlightingAttributeIds.CONSTANT_IDENTIFIER_ATTRIBUTE;
        private readonly ITreeNode myElement;

        public MyKeywordHighlighting(ITreeNode element)
        {
            myElement = element;
        }

        #region ICustomAttributeIdHighlighting Members

        public bool IsValid()
        {
            return true;
        }

        public string ToolTip
        {
            get { return null; }
        }

        public string ErrorStripeToolTip
        {
            get { return null; }
        }

        public int NavigationOffsetPatch
        {
            get { return 0; }
        }

        public string AttributeId
        {
            get { return AtributeId; }
        }

        #endregion

        #region IHighlightingWithRange Members

        public DocumentRange CalculateRange()
        {
            return myElement.GetNavigationRange();
        }

        #endregion
    }
}
