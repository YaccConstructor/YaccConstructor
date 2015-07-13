using System.Windows.Forms;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;
using JetBrains.DataFlow;
using JetBrains.UI.ToolWindowManagement;
using Microsoft.VisualStudio.Shell.Interop;
using JetBrains.ReSharper.Daemon;
using System;
using GraphX;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using System.Windows.Controls;
using GraphX.Controls;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting;

namespace YC.ReSharper.AbstractAnalysis.Plugin
{
  [ActionHandler("YC.ReSharper.AbstractAnalysis.Plugin.About")]
  public class AboutAction : IActionHandler
  {
    public bool Update(IDataContext context, ActionPresentation presentation, DelegateUpdate nextUpdate)
    {
      // return true or false to enable/disable this action
      return true;
    }

    public void Execute(IDataContext context, DelegateExecute nextExecute)
    {
        MessageBox.Show(
        "ReSharper.AbstractAnalysis\nJetBrains Lab\n\nAbstract analysis for string-embedded languages.",
        "About ReSharper.AbstractAnalysis",
        MessageBoxButtons.OK,
        MessageBoxIcon.Information);
    }
  }
}
