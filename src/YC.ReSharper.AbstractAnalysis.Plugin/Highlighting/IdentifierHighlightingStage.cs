using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.UsageChecking;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    [DaemonStage(StagesBefore = new[] { typeof(MySmartResolverStage) }, StagesAfter = new[] { typeof(CollectUsagesStage) })]
    public class IdentifierHighlightingStage : MyDaemonStageBase
    {
        public override IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            if (!IsSupported(process.SourceFile))
            {
                return EmptyList<IDaemonStageProcess>.InstanceList;
            }
            return new List<IDaemonStageProcess> { new IdentifierHighlighterProcess(process, settings, processKind) };
        }
    }
}
