using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Variable", null, HighlightingGroupIds.LanguageUsage, "Variable", @"
          Variable", Severity.INFO, false, Internal = false)]
namespace YC.ReSharper.AbstractAnalysis.Plugin.Inspections
{
    [ConfigurableSeverityHighlighting("Variable", "MyLang", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Variable")]
    internal class MyHighlighter : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private readonly string attributeId;
        private readonly ITreeNode myElement;

        public MyHighlighter(ITreeNode element)
        {
            myElement = element;
            attributeId = Helper.ColorDictionary.ContainsKey(element.GetText()) ? 
                Helper.ColorDictionary[element.GetText()] : HighlightingAttributeIds.ERROR_ATTRIBUTE;
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
            get { return attributeId; }
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
