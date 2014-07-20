/*
 * Copyright 2007-2011 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using System;
using JetBrains.Application.Progress;
using JetBrains.DocumentManagers;
using JetBrains.DocumentModel;
using JetBrains.IDE;
using JetBrains.ProjectModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.ExtensionsAPI;
using JetBrains.ReSharper.Psi.Files;
using System.Linq;
using JetBrains.ReSharper.Psi.Impl.Shared.InjectedPsi;
using JetBrains.ReSharper.Psi.Xml.XmlDocComments;
using JetBrains.TextControl;
using JetBrains.Util.dataStructures.TypedIntrinsics;
//using YC.ReSharper.AbstractAnalysis.Plugin.Core;
using YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow;

namespace YC.ReSharper.AbstractAnalysis.Plugin
{
    public class ComplexityAnalysisDaemonStageProcess : IDaemonStageProcess
    {
        private readonly IDaemonProcess myDaemonProcess;
        private readonly int myThreshold;

        public ComplexityAnalysisDaemonStageProcess(IDaemonProcess daemonProcess, int threshold)
        {
            myDaemonProcess = daemonProcess;
            myThreshold = threshold;
            //GraphLoader.InvokeLoadGrapFromCoreEvent += GetGraphs;
        }

        private YC.AbstractAnalysis.Helper.ReSharperHelper _processor = new YC.AbstractAnalysis.Helper.ReSharperHelper();

        public void Execute(Action<DaemonStageResult> commiter)
        {
            // Getting PSI (AST) for the file being highlighted
            var sourceFile = myDaemonProcess.SourceFile;
            var file = sourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) as ICSharpFile;
            
            if (file == null)
                return;

            // Running visitor against the PSI            
            var parserRes = _processor.Process(file);            
            // Checking if the daemon is interrupted by user activity
            if (myDaemonProcess.InterruptFlag)
            throw new ProcessCancelledException();

            var highlightings = (from e in parserRes.Item2 select new HighlightingInfo(e.Item2, new ComplexityWarning("Syntax error. Unexpected token " + e.Item1))).Concat(
                                from e in parserRes.Item1 select new HighlightingInfo(e.Item2, new ComplexityWarning("Unexpected symbol: " + e.Item1 + ".")));
            // Commit the result into document
            commiter(new DaemonStageResult(highlightings.ToArray()));
        }

        public void GetGraphs(object sender, EventArgs args)
        {
            try
            {
                //return graph to show
                //GraphLoader.OnEvent(this, new LoadGraphEventArgs(_processor));
            }
            catch (NullReferenceException e) { } // if GraphLoader is not created then nothing else to do
        }

        protected void F()
        {
            //InjectedPsiHolderNode.CreateHolderFor();
            //InjectedPsiExtensions.GetInjectAt();
            ////IInjectedPsiProvider
            //IndependentInjectedPsiProvider
        }

        public IDaemonProcess DaemonProcess
        {
            get { return myDaemonProcess; }
        }

        
    }
}