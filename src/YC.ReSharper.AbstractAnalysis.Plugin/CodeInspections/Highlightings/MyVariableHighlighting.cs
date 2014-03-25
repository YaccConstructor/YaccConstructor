using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("Variable", null, HighlightingGroupIds.LanguageUsage, "Variable", @"
          Variable", Severity.INFO, false, Internal = false)]
namespace Highlighting.Psi.CodeInspections.Highlightings
{
    [ConfigurableSeverityHighlighting("Variable", "CSharp", OverlapResolve = OverlapResolveKind.NONE, ToolTipFormatString = "Variable")]
    internal class MyVariableHighlighting : ICustomAttributeIdHighlighting, IHighlightingWithRange
    {
        private const string AtributeId = HighlightingAttributeIds.LOCAL_VARIABLE_IDENTIFIER_ATTRIBUTE;
        private readonly ITreeNode myElement;

        public MyVariableHighlighting(ITreeNode element)
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
            get { return "null"; }
        }

        public string ErrorStripeToolTip
        {
            get { return "null"; }
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
