namespace VSYardNS

open System.Collections.Generic
open System
open System.ComponentModel.Composition
open SolutionData
open EnvDTE

module DataHelper =
    let ReParseFileForActiveWindow(m_dte: EnvDTE.DTE, text : string) = 
            let dte = m_dte
            let activeSolutionProjects = dte.ActiveSolutionProjects :?> Array
            let activeProject =  activeSolutionProjects.GetValue(0) :?> EnvDTE.Project
            let yaFile = dte.ActiveDocument
            let solution1 = SolutionData.GetSolution()
            let projectFileName = activeProject.Properties.Item("FileName").Value.ToString()
            let yardFileName = yaFile.Name
            solution1.ReParseFile(projectFileName,yardFileName, text)

    let GetFileForActiveWindow(m_dte: EnvDTE.DTE) = 
            let dte = m_dte
            let activeSolutionProjects = dte.ActiveSolutionProjects :?> Array
            let activeProject =  activeSolutionProjects.GetValue(0) :?> EnvDTE.Project
            let yaFile = dte.ActiveDocument
            let solution1 = SolutionData.GetSolution()
            let projectFileName = activeProject.Properties.Item("FileName").Value.ToString()
            let yardFileName = yaFile.Name
            solution1.GetParseFile(projectFileName,yardFileName)

    let GetProjectForActiveWindow(m_dte: EnvDTE.DTE) = 
            let dte = m_dte
            let activeSolutionProjects = dte.ActiveSolutionProjects :?> Array
            let activeProject =  activeSolutionProjects.GetValue(0) :?> EnvDTE.Project
            let solution1 = SolutionData.GetSolution()
            let projectFileName = activeProject.Properties.Item("FileName").Value.ToString()
            solution1.GetParseProject(projectFileName)