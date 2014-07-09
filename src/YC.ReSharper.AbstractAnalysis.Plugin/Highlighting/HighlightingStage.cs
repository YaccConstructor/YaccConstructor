using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
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
        public IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            if (!IsSupported(process.SourceFile))
            {
                return EmptyList<IDaemonStageProcess>.InstanceList;
            }
            return new List<IDaemonStageProcess> { new HighlightingProcess(process, settings, processKind) };
        }

        public ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile, IContextBoundSettingsStore settingsStore)
        {
            return ErrorStripeRequest.NONE;
        }

        public bool IsSupported(IPsiSourceFile sourceFile)
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
