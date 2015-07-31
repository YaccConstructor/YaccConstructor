using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi.Tree;

using ReSharperExtension.Highlighting;
using ReSharperExtension.YcIntegration;

[assembly: RegisterConfigurableSeverity(TokenHighlighting.SEVERITY_ID, null, HighlightingGroupIds.LanguageUsage, TokenHighlighting.SEVERITY_ID, TokenHighlighting.SEVERITY_ID, 
    Severity.INFO, false, Internal = false)]
namespace ReSharperExtension.Highlighting
{
    [ConfigurableSeverityHighlighting(SEVERITY_ID, LANGUAGE_NAME, OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = null)]
    internal class TokenHighlighting : ICustomAttributeIdHighlighting
    {
        //public const string HIGHLIGHTING_ID = "YC.SEL.SDK Highlighting";
        public const string SEVERITY_ID = "YC.SEL.SDK Highlighting";
        public const string LANGUAGE_NAME = "Embedded language";

        private readonly string attributeId;
        private readonly ITreeNode myElement;

        public TokenHighlighting(ITreeNode element)
        {
            myElement = element;
            string lang = element.UserData.GetData(Constants.YcLanguage);
            string tokenName = element.UserData.GetData(Constants.YcTokenName);
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

        public DocumentRange CalculateRange()
        {
            return myElement.GetNavigationRange();
        }
        
        #endregion
    }
}