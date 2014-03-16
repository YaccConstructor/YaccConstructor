using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.CodeInspections;

namespace Highlighting.CodeInspections
{
    [DaemonStage(StagesBefore = new[] { typeof(GlobalFileStructureCollectorStage) }, StagesAfter = new[] { typeof(IdentifierHighlightingStage) })]
    public class SmartResolverStage : MyDaemonStageBase
    {
        public override IEnumerable<IDaemonStageProcess> CreateProcess(IDaemonProcess process, IContextBoundSettingsStore settings, DaemonProcessKind processKind)
        {
            if (!IsSupported(process.SourceFile))
            {
                return EmptyList<IDaemonStageProcess>.InstanceList;
            }
            return new List<IDaemonStageProcess> { new SmartResolverProcess(process) };
        }
    }
}
