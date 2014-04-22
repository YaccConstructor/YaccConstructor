using System;
using JetBrains.ReSharper.Daemon;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public class MySmartResolverProcess : IDaemonStageProcess
    {
        private IDaemonProcess myProcess;

        public MySmartResolverProcess(IDaemonProcess process)
        {
            myProcess = process;
        }
        
        public void Execute(Action<DaemonStageResult> committer)
        {
            
        }

        public IDaemonProcess DaemonProcess
        {
            get
            {
                return myProcess;
            }
        }
    }
}