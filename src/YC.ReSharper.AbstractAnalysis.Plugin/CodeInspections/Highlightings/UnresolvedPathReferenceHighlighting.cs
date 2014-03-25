using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Highlighting.Tree.Impl;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Impl;
using JetBrains.ReSharper.Psi.Resolve;
using JetBrains.ReSharper.Psi.Tree;

[assembly: RegisterConfigurableSeverity("UnresolvedReference", null, HighlightingGroupIds.LanguageUsage, "Unresolved reference", @"
          Unresolved reference", Severity.ERROR, false, Internal = false)]
namespace Highlighting.Psi.CodeInspections.Highlightings
{
    [ConfigurableSeverityHighlighting("UnresolvedReference", "CSharp", OverlapResolve = OverlapResolveKind.ERROR, ToolTipFormatString = Error)]
  internal class UnresolvedPathReferenceHighlighting : IHighlightingWithRange //C:\Program Files (x86)\JetBrains\ReSharper\v8.0\SDK\Samples\PsiPlugin\src\CodeInspections\Psi\Highlightings\PsiUnresolvedPathReferenceHighlighting.cs
  {
    private const string Error = "Unresolved reference";
    private readonly ITreeNode myElement;
    private IReference myReference;

    public UnresolvedPathReferenceHighlighting(PathName element)
    {
      myElement = element;

      myReference = element.Reference;

    }

    #region IHighlightingWithRange Members

    public bool IsValid()
    {
      return true;
    }

    public string ToolTip
    {
      get { return Error; }
    }

    public string ErrorStripeToolTip
    {
      get { return Error; }
    }

    public int NavigationOffsetPatch
    {
      get { return 0; }
    }

    public DocumentRange CalculateRange()
    {
      return myElement.GetNavigationRange();
    }

    #endregion

    public IReference Reference
    {
      get { return myReference; }
    }


  }
}
