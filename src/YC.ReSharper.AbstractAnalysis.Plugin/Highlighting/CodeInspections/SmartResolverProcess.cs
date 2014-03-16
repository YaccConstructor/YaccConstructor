using System;
using JetBrains.ReSharper.Daemon;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.CodeInspections
{
    public class SmartResolverProcess : IDaemonStageProcess
    {
        private IDaemonProcess myProcess;

        //public SmartResolverProcess(IDaemonProcess process, ITreeNode treeNode)
        //{
        //    myProcess = process;
        //}

        public SmartResolverProcess(IDaemonProcess process)
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