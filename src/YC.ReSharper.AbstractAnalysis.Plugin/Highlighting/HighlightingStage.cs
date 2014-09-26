using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [DaemonStage]
    public class HighlightingStage : IDaemonStage
    {
        private static Dictionary<IDocument, HighlightingProcess> documentToProcess = new Dictionary<IDocument, HighlightingProcess>();

        public IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            Handler.Init();
            if (processKind != DaemonProcessKind.VISIBLE_DOCUMENT ||
                !IsSupported(process.SourceFile))
            {
                return EmptyList<IDaemonStageProcess>.InstanceList;
            }

            var document = process.Document;
            if (!documentToProcess.ContainsKey(document))
            {
                var highlightingProcess = new HighlightingProcess(process, settings);
                documentToProcess.Add(process.Document, highlightingProcess);
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

        private bool IsSupported(IPsiSourceFile sourceFile)
        {
            if (sourceFile == null || !sourceFile.IsValid())
            {
                return false;
            }
            IPsiServices psiServices = sourceFile.GetPsiServices();
            IFile psiFile = psiServices.Files.GetDominantPsiFile<CSharpLanguage>(sourceFile);
            return psiFile != null && psiFile.Language.Is<CSharpLanguage>();
        }
    }
}
