using Highlighting.Core;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Variable", null, HighlightingGroupIds.LanguageUsage, "Variable", @"
          Variable", Severity.INFO, false, Internal = false)]
namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [ConfigurableSeverityHighlighting("Variable", "MyLang", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Variable")]
    internal class MyLeafHighlighting : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private string attributeId;
        private readonly ITreeNode myElement;

        public MyLeafHighlighting(ITreeNode element)
        {
            myElement = element;
            string ycTokName = element.UserData.GetData(KeyConstant.YcTokName);
            if (ColorHelper.TokenToColor == null || string.IsNullOrEmpty(ycTokName))
            {
                attributeId = ColorHelper.DefaultColor;
            }

            else if (ColorHelper.TokenToColor.ContainsKey(ycTokName))
            {
                attributeId = ColorHelper.TokenToColor[ycTokName];
            }
            else
            {
                attributeId = ColorHelper.DefaultColor;
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
