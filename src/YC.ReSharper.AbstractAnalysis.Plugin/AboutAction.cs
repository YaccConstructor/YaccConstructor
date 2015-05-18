using System.Windows.Forms;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;

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
