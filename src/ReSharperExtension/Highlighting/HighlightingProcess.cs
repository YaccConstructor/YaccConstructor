using System;
using System.Collections.Generic;

using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.JavaScript.LanguageImpl;
using JetBrains.ReSharper.Psi.JavaScript.Tree;
using JetBrains.ReSharper.Psi.Tree;
using ReSharperExtension.Highlighting.Dynamic;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting
{
    public class HighlightingProcess : IDaemonStageProcess
    {
        private Action<DaemonStageResult> myCommiter;
        private IContextBoundSettingsStore mySettingsStore;

        private static ReSharperHelper<DocumentRange, ITreeNode> YcProcessor = ReSharperHelper<DocumentRange, ITreeNode>.Instance;

        public IDaemonProcess DaemonProcess { get; private set; }
        public IHighlightingConsumer Consumer
        {
            get
            {
                return new DefaultHighlightingConsumer(this, mySettingsStore);
            }
        }

        public IFile File
        {
            get { return GetFile(); }
        }

        private IFile GetFile()
        {
            IFile csFile = GetcsFile();
            if (csFile != null)
                return csFile;

            IFile jsFile = GetJsFile();
            if (jsFile != null)
                return jsFile;
            
            return null;
        }

        private IFile GetcsFile()
        {
            IPsiServices psiServices = DaemonProcess.SourceFile.GetPsiServices();
            return psiServices.Files.GetDominantPsiFile<CSharpLanguage>(DaemonProcess.SourceFile) as ICSharpFile;
        }

        private IFile GetJsFile() 
        {
            IPsiServices psiServices = DaemonProcess.SourceFile.GetPsiServices();
            return psiServices.Files.GetDominantPsiFile<JavaScriptLanguage>(DaemonProcess.SourceFile) as IJavaScriptFile;
        }

        public HighlightingProcess(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            DaemonProcess = process;
            mySettingsStore = settingsStore;
        }

        public void Update(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            DaemonProcess = process;
            mySettingsStore = settingsStore;
        }

        public void Execute(Action<DaemonStageResult> commiter)
        {
            if (File == null)
                return;

            myCommiter = commiter;
            UpdateHandler();

            ExistingTreeNodes.ClearExistingTree(DaemonProcess.Document);
            var errors = YcProcessor.Process(File);
            OnErrors(errors);
            // remove all old highlightings
            //if (DaemonProcess.FullRehighlightingRequired)
            //myCommiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }

        private void UpdateHandler()
        {
            Handler.Process = this;
        }

        private void OnErrors(ProcessErrors errors)
        {
            var highlightings = new List<HighlightingInfo>();

            List<ErrorInfo> parserErrors = errors.ParserErrors;
            foreach (ErrorInfo error in parserErrors)
            {
                var newHighlighting = 
                    new HighlightingInfo(error.Range, new ErrorWarning(error.Range, "Syntax error. Unexpected token "));
                highlightings.Add(newHighlighting);
            }

            List<ErrorInfo> lexerErrors = errors.LexerErrors;
            foreach (ErrorInfo error in lexerErrors)
            {
                var newHighlighting = 
                    new HighlightingInfo(error.Range, new ErrorWarning(error.Range, String.Format("Unexpected symbol: {0} .", error.Message)));
                highlightings.Add(newHighlighting);
            }

            List<ErrorInfo> semanticErrors = errors.SemanticErrors;
            foreach (ErrorInfo error in semanticErrors)
            {
                var newHighlighting =
                    new HighlightingInfo(error.Range, new ErrorWarning(error.Range, String.Format("Semantic error. Symbol: {0} .", error.Message)));
                highlightings.Add(newHighlighting);
            }

            
            //var highlightings = (from e in errors.Item2 select new HighlightingInfo(e.Item2, new ErrorWarning())).Concat(
            //                    from e in errors.Item1 select new HighlightingInfo(e.Item2, new ErrorWarning("Unexpected symbol: " + e.Item1 + ".")));
            DoHighlighting(new DaemonStageResult(highlightings));
        }

        public void DoHighlighting(DaemonStageResult result)
        {
            myCommiter(result);
        }
    }
}
