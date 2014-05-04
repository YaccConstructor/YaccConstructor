using System;
using Highlighting.Core;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public abstract class MyDaemonStageProcessBase : ITreeNodeVisitor<IHighlightingConsumer>,
        IRecursiveElementProcessor<IHighlightingConsumer>, IDaemonStageProcess
    {
        public IDaemonProcess myDaemonProcess;
        public IFile myFile;

        public IContextBoundSettingsStore mySettingsStore;

        protected MyDaemonStageProcessBase(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            myDaemonProcess = process;
            myFile = MyDaemonStageBase.GetPsiFile(myDaemonProcess.SourceFile);
            mySettingsStore = settingsStore;
        }

        public IFile File
        {
            get { return myFile; }
        }

        #region IDaemonStageProcess Members

        public IDaemonProcess DaemonProcess
        {
            get { return myDaemonProcess; }
        }

        public abstract void Execute(Action<DaemonStageResult> commiter);

        #endregion

        #region IRecursiveElementProcessor<IHighlightingConsumer> Members

        public virtual bool InteriorShouldBeProcessed(ITreeNode element, IHighlightingConsumer context)
        {
            return true;
        }

        public bool IsProcessingFinished(IHighlightingConsumer context)
        {
            return false;
        }


        public virtual void ProcessBeforeInterior(ITreeNode element, IHighlightingConsumer consumer)
        {
        }

        public virtual void ProcessAfterInterior(ITreeNode element, IHighlightingConsumer consumer)
        {
            if (element.FirstChild == null)
                VisitSomething(element, consumer);
            
        }
        #endregion

        public virtual void VisitSomething(ITreeNode node, IHighlightingConsumer consumer)
        {}
    
        protected void HighlightInFile(Action<IFile, IHighlightingConsumer> fileHighlighter, Action<DaemonStageResult> commiter)
        {
            var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
            fileHighlighter(File, consumer);
            commiter(new DaemonStageResult(consumer.Highlightings));
        }
    }
}
