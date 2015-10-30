using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.ActionManagement;
using JetBrains.Application.DataContext;
using JetBrains.UI.ActionsRevised;
using JetBrains.UI.MenuGroups;

namespace ReSharperExtension
{
    [ActionGroup(ActionGroupInsertStyles.Embedded)]
    public class YcGroup : IAction, IInsertLast<VsMainMenuGroup>
    {
        public YcGroup(Separator sep, AboutActionHandler handler)
        {
        }
    }

    [Action("YC.Plugin", Id = 12345678)]
    public class AboutActionHandler : IExecutableAction
    {
        public bool Update(IDataContext context, ActionPresentation presentation, DelegateUpdate nextUpdate)
        {
            return true;
        }

        public void Execute(IDataContext context, DelegateExecute nextExecute)
        {
            return;
        }
    }
}
