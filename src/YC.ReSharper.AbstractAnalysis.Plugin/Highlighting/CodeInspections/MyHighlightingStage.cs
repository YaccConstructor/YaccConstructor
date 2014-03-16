using System.Collections.Generic;
using Highlighting.CodeInspections;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.CodeInspections
{
    public class MyHighlightingStage : MyDaemonStageBase 
    {
        public override ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile,
            IContextBoundSettingsStore settings)
        {
            return ErrorStripeRequest.STRIPE_AND_ERRORS;
        }

        public override IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process,
            IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            return new List<IDaemonStageProcess> {new IdentifierHighlighterProcess(process, settings)};
        }

    }
}
