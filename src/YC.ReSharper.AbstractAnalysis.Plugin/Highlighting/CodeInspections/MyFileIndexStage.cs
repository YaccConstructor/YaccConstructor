using System.Collections.Generic;
using Highlighting.CodeInspections;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.CodeInspections
{
    [DaemonStage]
    public class MyFileIndexStage : MyDaemonStageBase
    {
        public override ErrorStripeRequest NeedsErrorStripe(IPsiSourceFile sourceFile, IContextBoundSettingsStore settings)
        {
            return IsSupported(sourceFile) ? ErrorStripeRequest.STRIPE_AND_ERRORS : ErrorStripeRequest.NONE;
        }

        public override IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {

            if (!IsSupported(process.SourceFile))
            {
                return EmptyList<IDaemonStageProcess>.InstanceList;
            }
            return new List<IDaemonStageProcess> { new MyFileIndexProcess(process, settings) };
        }
    }
}
