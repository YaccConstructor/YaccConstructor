using System.Windows.Forms;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;

namespace ReSharperExtension
{
    [ActionHandler("ReSharperExtension.About")]
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
              "2\nAcme Corp.\n\n",
              "About 2",
              MessageBoxButtons.OK,
              MessageBoxIcon.Information);
        }
    }
}