using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi.Tree;

using ReSharperExtension.Highlighting.Dynamic;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting
{
    internal class HighlightingProcess : IDaemonStageProcess
    {
        private Action<DaemonStageResult> myCommiter;
        private IContextBoundSettingsStore mySettingsStore;

        private static readonly ReSharperHelper<DocumentRange, ITreeNode> YcProcessor = ReSharperHelper<DocumentRange, ITreeNode>.Instance;

        public HighlightingProcess(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            DaemonProcess = process;
            mySettingsStore = settingsStore;
        }

        #region IDaemonStageProcess members
        public IDaemonProcess DaemonProcess { get; private set; }

        public void Execute(Action<DaemonStageResult> commiter)
        {
            if (File == null)
                return;

            myCommiter = commiter;
            Handler.Process = this;

            ExistingRanges.ClearExistingRanges(DaemonProcess.Document);
            ProcessErrors errors = YcProcessor.Process(File);
            HighlightErrors(errors);
            // remove all old highlightings
            //if (DaemonProcess.FullRehighlightingRequired)
            //myCommiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }
        #endregion

        public IHighlightingConsumer Consumer
        {
            get { return new DefaultHighlightingConsumer(this, mySettingsStore); }
        }

        private IFile File
        {
            get { return HostLanguageHelper.GetFile(DaemonProcess.SourceFile); }
        }

        public void Update(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            DaemonProcess = process;
            mySettingsStore = settingsStore;
        }

        public void DoHighlighting(DaemonStageResult result)
        {
            myCommiter(result);
        }

        private void HighlightErrors(ProcessErrors errors)
        {
            List<ErrorInfo> allErrors = errors.GetAllErrors();

            Func<ErrorInfo, HighlightingInfo> toHighlightingInfoFunc =
                error => new HighlightingInfo(error.Range, new ErrorWarning(error.Range, error.Message));

            List<HighlightingInfo> highlightings = allErrors.Select(toHighlightingInfoFunc).ToList();

            //var highlightings = (from e in errors.Item2 select new HighlightingInfo(e.Item2, new ErrorWarning())).Concat(
            //                    from e in errors.Item1 select new HighlightingInfo(e.Item2, new ErrorWarning("Unexpected symbol: " + e.Item1 + ".")));
            DoHighlighting(new DaemonStageResult(highlightings));
        }
    }
}