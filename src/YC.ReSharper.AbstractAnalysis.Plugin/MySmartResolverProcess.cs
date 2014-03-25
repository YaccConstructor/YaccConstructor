using System;
using JetBrains.ReSharper.Daemon;

namespace YC.ReSharper.AbstractAnalysis.Plugin
{
    public class MySmartResolverProcess : IDaemonStageProcess
    {
        private IDaemonProcess myProcess;

        //public MySmartResolverProcess(IDaemonProcess process, ITreeNode treeNode)
        //{
        //    myProcess = process;
        //}

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