using System.Collections.Generic;

using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.Util;

namespace ReSharperExtension.Highlighting
{
    [DaemonStage]
    public class HighlightingStage : IDaemonStage
    {
        private static Dictionary<IDocument, HighlightingProcess> documentToProcess = new Dictionary<IDocument, HighlightingProcess>();

        #region IDaemonStage members

        public IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            Handler.Init();
            if (processKind != DaemonProcessKind.VISIBLE_DOCUMENT ||
                !HostLanguageHelper.IsSupportedFile(process.SourceFile))
            {
                return EmptyList<IDaemonStageProcess>.InstanceList;
            }

            IDocument document = process.Document;
            if (!documentToProcess.ContainsKey(document))
            {
                var highlightingProcess = new HighlightingProcess(process, settings);
                documentToProcess.Add(document, highlightingProcess);
            }
            else
            {
                documentToProcess[document].Update(process, settings);
            }

            return new List<IDaemonStageProcess> { documentToProcess[document] };
        }

        public ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile, IContextBoundSettingsStore settingsStore)
        {
            return ErrorStripeRequest.NONE;
        }

        #endregion
        
    }
}
