using System;
using System.IO;
using JetBrains.Application.Settings;
using JetBrains.Application.Threading;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace Highlighting.CodeInspections
{
    public abstract class MyIncrementalDaemonStageProcessBase : MyDaemonStageProcessBase
    {
        private readonly IDaemonProcess myProcess;
        private readonly IContextBoundSettingsStore mySettingsStore;

        protected MyIncrementalDaemonStageProcessBase(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
            : base(process, settingsStore)
        {
            myProcess = process;
            mySettingsStore = settingsStore;
        }

        public override void Execute(Action<DaemonStageResult> commiter)
        {
            Action globalHighlighter = () =>
            {
                var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
                ProcessThisAndDescendants(File, new ProcessorBase(this, consumer));
                //File.ProcessThisAndDescendants(new GlobalProcessor(this, consumer));
                commiter(new DaemonStageResult(consumer.Highlightings) {Layer = 1});
            };

            using (IMultiCoreFibers fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(globalHighlighter);
            }

            // remove all old highlightings
            //commiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }

        private void ProcessThisAndDescendants(IFile file, IRecursiveElementProcessor processor)
        {
            var treeNode = TreeNodeHolder.TreeNode;
            //processor.ProcessBeforeInterior(treeNode);
            if (processor.InteriorShouldBeProcessed(treeNode))
            {
                ProcessDescendants(treeNode, processor);
            }
            //processor.ProcessAfterInterior(treeNode);
        }

        private void ProcessDescendants(ITreeNode root, IRecursiveElementProcessor processor)
        {
            var treeNode = root;
            if (treeNode == null)
            {
                return;
            }
            while (!processor.ProcessingIsFinished)
            {
                processor.ProcessBeforeInterior(treeNode);
                //CompositeElement compositeElement = treeNode as CompositeElement;
                //if (compositeElement != null && compositeElement.firstChild != null && processor.InteriorShouldBeProcessed(treeNode))
                //{
                //    treeNode = treeNode.FirstChild;
                //}
                if (processor.InteriorShouldBeProcessed(treeNode) && treeNode.FirstChild != null)
                {
                    treeNode = treeNode.FirstChild;
                }
                else
                {
                    while (treeNode.NextSibling == null)
                    {
                        processor.ProcessAfterInterior(treeNode);
                        treeNode = treeNode.Parent;
                        if (treeNode == root || treeNode == null)
                        {
                            return;
                        }
                    }
                    processor.ProcessAfterInterior(treeNode);
                    treeNode = treeNode.NextSibling;
                }
            }
        }
        
        #region Nested type: ProcessorBase

        private class ProcessorBase : IRecursiveElementProcessor
        {
            private readonly IHighlightingConsumer myConsumer;
            private readonly MyDaemonStageProcessBase myProcess;

            public ProcessorBase(MyDaemonStageProcessBase process, IHighlightingConsumer consumer)
            {
                myProcess = process;
                myConsumer = consumer;
            }

            #region IRecursiveElementProcessor Members

            public bool ProcessingIsFinished
            {
                get { return myProcess.IsProcessingFinished(myConsumer); }
            }

            public virtual void ProcessBeforeInterior(ITreeNode element)
            {
                myProcess.ProcessBeforeInterior(element, myConsumer);
            }

            public virtual void ProcessAfterInterior(ITreeNode element)
            {
                myProcess.ProcessAfterInterior(element, myConsumer);
            }

            public virtual bool InteriorShouldBeProcessed(ITreeNode element)
            {
                return myProcess.InteriorShouldBeProcessed(element, myConsumer);
            }

            #endregion
        }

        #endregion
    }
}
