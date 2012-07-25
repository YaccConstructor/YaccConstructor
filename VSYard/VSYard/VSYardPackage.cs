using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using System.Collections.Generic;
using Microsoft.Win32;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using VSYardNS;

namespace YC.VSYard
{

    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    ///
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the 
    /// IVsPackage interface and uses the registration attributes defined in the framework to 
    /// register itself and its components with the shell.
    /// </summary>
    // This attribute tells the PkgDef creation utility (CreatePkgDef.exe) that this class is
    // a package.
    [PackageRegistration(UseManagedResourcesOnly = true)]
    // This attribute is used to register the informations needed to show the this package
    // in the Help/About dialog of Visual Studio.
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    [Guid(GuidList.guidVSYardPkgString)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string)]
    public sealed class VSYardPackage : Package, IVsSolutionEvents, IVsSolutionEvents2, IVsSolutionEvents3, IVsSolutionEvents4, IVsSolutionEventsProjectUpgrade
    {

        uint m_solutionCookie = 0;

        /// <summary>
        /// Default constructor of the package.
        /// Inside this method you can place any initialization code that does not require 
        /// any Visual Studio service because at this point the package object is created but 
        /// not sited yet inside Visual Studio environment. The place to do all the other 
        /// initialization is the Initialize method.
        /// </summary>
        public VSYardPackage()
        {
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering constructor for: {0}", this.ToString()));
        }



        /////////////////////////////////////////////////////////////////////////////
        // Overriden Package Implementation
        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initilaization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            Trace.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this.ToString()));
            base.Initialize();
            IVsSolution solution = (IVsSolution)GetService(typeof(SVsSolution));
            ErrorHandler.ThrowOnFailure(solution.AdviseSolutionEvents(this, out m_solutionCookie));
        }
        #endregion


        public int OnAfterCloseSolution(object pUnkReserved)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterLoadProject(IVsHierarchy pStubHierarchy, IVsHierarchy pRealHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterOpenProject(IVsHierarchy pHierarchy, int fAdded)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            var m_dte = (EnvDTE.DTE)this.GetService(typeof(EnvDTE.DTE));

            var w = m_dte.Solution.Projects.Item("").ProjectItems.Item("").Open();
    //        m_dte.ActiveSolutionProjects
     //       w.Activate();

            /*
            Открыть докумнет: (наверное)
            m_dte.Solution.Projects.Item("").ProjectItems.Item("").Open
            
            Активный документ:
            m_dte.ActiveDocument.ExtenderCATID.ToString();
            */
            if (m_dte == null)
                ErrorHandler.ThrowOnFailure(1);
            if (m_dte.Solution != null)
            {
                Dictionary<string, SolutionData.Project> projects = new Dictionary<string, SolutionData.Project>(); // Создание списка проектов
                
                foreach (EnvDTE.Project i in m_dte.Solution.Projects)
                {   
                    string extenderCATID = i.Properties.Item("ExtenderCATID").Value.ToString();
                    string fileName = i.Properties.Item("FileName").Value.ToString();
                    string fullPath =  i.Properties.Item("FullPath").Value.ToString();
                    string rootYard = null;
                    Dictionary<string, SolutionData.YardFile> dictionaryOfYard = new Dictionary<string, SolutionData.YardFile>();
                    

                    foreach (EnvDTE.ProjectItem pi in i.ProjectItems)
                    {
                        if (pi.Properties.Item("Extension").Value.ToString() == ".yrd")
                        {
                            string yardFileName = pi.Properties.Item("FileName").Value.ToString();
                            string yardExtenderCATID = pi.Properties.Item("ExtenderCATID").Value.ToString();
                            string yardFullPath = pi.Properties.Item("FullPath").Value.ToString();
                            dictionaryOfYard.Add(yardExtenderCATID,
                                                 new SolutionData.YardFile(new SolutionData.YardInfo(yardExtenderCATID,
                                                                                                     yardFileName,
                                                                                                     yardFullPath)));
                            if(rootYard == null)
                            {
                                rootYard = yardExtenderCATID;
                            }
                        }
                    }
                    SolutionData.Project project = new SolutionData.Project(new SolutionData.ProjectInfo(extenderCATID, fileName,fullPath, rootYard, dictionaryOfYard));
                    projects.Add(extenderCATID, project);
                }

                SolutionData.Solution solution = SolutionData.GetSolution();
                solution.FirstRunAddProjects(projects);
            }
            return VSConstants.S_OK;
        }

        public int OnBeforeCloseProject(IVsHierarchy pHierarchy, int fRemoved)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnBeforeCloseSolution(object pUnkReserved)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnBeforeUnloadProject(IVsHierarchy pRealHierarchy, IVsHierarchy pStubHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnQueryCloseProject(IVsHierarchy pHierarchy, int fRemoving, ref int pfCancel)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnQueryCloseSolution(object pUnkReserved, ref int pfCancel)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnQueryUnloadProject(IVsHierarchy pRealHierarchy, ref int pfCancel)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }


        public int OnAfterClosingChildren(IVsHierarchy pHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterMergeSolution(object pUnkReserved)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterOpeningChildren(IVsHierarchy pHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnBeforeClosingChildren(IVsHierarchy pHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnBeforeOpeningChildren(IVsHierarchy pHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterUpgradeProject(IVsHierarchy pHierarchy, uint fUpgradeFlag, string bstrCopyLocation, SYSTEMTIME stUpgradeTime, IVsUpgradeLogger pLogger)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterAsynchOpenProject(IVsHierarchy pHierarchy, int fAdded)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterChangeProjectParent(IVsHierarchy pHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnAfterRenameProject(IVsHierarchy pHierarchy)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }

        public int OnQueryChangeProjectParent(IVsHierarchy pHierarchy, IVsHierarchy pNewParentHier, ref int pfCancel)
        {
            //throw new NotImplementedException();
            return VSConstants.S_OK;
        }
    }
}
