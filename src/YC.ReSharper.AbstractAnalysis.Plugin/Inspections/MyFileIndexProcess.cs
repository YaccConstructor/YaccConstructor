using System;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Psi;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Inspections
{
    public class MyFileIndexProcess : MyDaemonStageProcessBase
    {
        public MyFileIndexProcess(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
            : base(process, settingsStore)
        {
        }

        public override void Execute(Action<DaemonStageResult> commiter)
        {
            HighlightInFile((file, consumer) => file.ProcessDescendants(this, consumer), commiter);
        }
    }
}
