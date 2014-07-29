using Highlighting.Core;
using JetBrains.Annotations;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Variable", null, HighlightingGroupIds.LanguageUsage, "Variable", @"
          Variable", Severity.INFO, false, Internal = false)]
namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [ConfigurableSeverityHighlighting("Variable", "MyLang", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Variable")]
    internal class TokenHighlighting : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private readonly string attributeId;
        private readonly ITreeNode myElement;

        public TokenHighlighting(ITreeNode element)
        {
            myElement = element;
            string lang = element.UserData.GetData(KeyConstant.YcLanguage);
            string tokenName = element.UserData.GetData(KeyConstant.YcTokenName);
            attributeId = LanguageHelper.GetColor(lang, tokenName);
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