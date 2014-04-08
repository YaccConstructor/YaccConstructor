using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Variable", null, HighlightingGroupIds.LanguageUsage, "Variable", @"
          Variable", Severity.INFO, false, Internal = false)]
namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [ConfigurableSeverityHighlighting("Variable", "MyLang", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Variable")]
    internal class MySomethingHighlighting : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private string attributeId;
        //private const string AtributeId = HighlightingAttributeIds.CONSTANT_IDENTIFIER_ATTRIBUTE;
        private readonly ITreeNode myElement;

        public MySomethingHighlighting(ITreeNode element)
        {
            myElement = element;
            var tokenName = element.GetText();
            if (TreeNodeHolder.TokenToColor.ContainsKey(tokenName))
            {
                attributeId = TreeNodeHolder.TokenToColor[tokenName];
            }
            else
            {
                attributeId = TreeNodeHolder.DefaultColor;
            }
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
