using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using EnvDTE;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

namespace YC.VSYard.Helpers
{
    class SolutionNavigatorHelper
    {
        internal static string GetRootYrd(Project project)
        {
            foreach (var itm in project.ProjectItems)
            {
                var pItem = itm as ProjectItem;
                if (pItem != null && pItem.Name.EndsWith(".yrd")) return pItem.Name;
            }
            return string.Empty;
        }

        internal static Project GetActiveProject(DTE dte)
        {
            Project activeProject = null;

            Array activeSolutionProjects = dte.ActiveSolutionProjects as Array;
            if (activeSolutionProjects != null && activeSolutionProjects.Length > 0)
            {
                activeProject = activeSolutionProjects.GetValue(0) as Project;
            }            

            return activeProject;
        }

        internal static Project GetActiveProject()
        {
            DTE dte = Package.GetGlobalService(typeof(SDTE)) as DTE;
            return GetActiveProject(dte);
        }
    }
}
