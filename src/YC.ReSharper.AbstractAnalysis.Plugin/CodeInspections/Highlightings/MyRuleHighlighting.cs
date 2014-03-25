using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Rule", null, HighlightingGroupIds.LanguageUsage, "Rule", @"
          Rule", Severity.INFO, false, Internal = false)]
namespace Highlighting.Psi.CodeInspections.Highlightings
{
    [ConfigurableSeverityHighlighting("Rule", "CSharp", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Rule")]
    internal class MyRuleHighlighting : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private const string Attribute = HighlightingAttributeIds.METHOD_IDENTIFIER_ATTRIBUTE;
        private readonly ITreeNode myElement;

        public MyRuleHighlighting(ITreeNode element)
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
            get { return Attribute; }
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
