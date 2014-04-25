using System;
using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;
using JetBrains.Application.Progress;
using JetBrains.Application.Settings;
using JetBrains.Application.Threading;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public abstract class MyIncrementalDaemonStageProcessBase : MyDaemonStageProcessBase
    {
        private readonly IDaemonProcess myProcess;
        private readonly DaemonProcessKind myProcessKind;
        private YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor ycProcessor;

        protected MyIncrementalDaemonStageProcessBase(IDaemonProcess process, IContextBoundSettingsStore settingsStore, DaemonProcessKind processKind)
            : base(process, settingsStore)
        {
            myProcess = process;
            mySettingsStore = settingsStore;
            myProcessKind = processKind;
        }

        private void UpdateYCProcessor()
        {
            // Getting PSI (AST) for the file being highlighted
            var sourceFile = myDaemonProcess.SourceFile;
            var file = sourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) as ICSharpFile;
            if (file == null)
                return;

            // Running visitor against the PSI
            ycProcessor = new YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor(file);
            ycProcessor.Process();

            TreeNodeHolder.ParseFile(ycProcessor.XmlPath);
        }

        public override void Execute(Action<DaemonStageResult> commiter)
        {
            if (myProcessKind != DaemonProcessKind.VISIBLE_DOCUMENT)
                return;

            Action globalHighlighter = () =>
            {
                var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
                ProcessThisAndDescendants(new ProcessorBase(this, consumer));
                commiter(new DaemonStageResult(consumer.Highlightings) {Layer = 1});
            };
            
            using (var fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(globalHighlighter);
            }

            // remove all old highlightings
            //commiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }

        private void ProcessThisAndDescendants(IRecursiveElementProcessor processor)
        {
            UpdateYCProcessor();

            var fsTree = ycProcessor.GetNextForest<IEnumerable<IAbstractTreeNode>>();
            // fsTree is List<ITreeNode>. It can be null.
            while (fsTree != null)
            {
                var tree = (fsTree.ToList())[0];
                ProcessDescendants(tree, processor);
                fsTree = ycProcessor.GetNextForest<IEnumerable<IAbstractTreeNode>>();
            }
        }

        private void ProcessDescendants(ITreeNode root, IRecursiveElementProcessor processor)
        {
            var treeNode = root as IAbstractTreeNode;
            if (treeNode == null)
            {
                return;
            }
            while (!processor.ProcessingIsFinished)
            {
                if (treeNode.FirstChild != null)
                {
                    treeNode = treeNode.FirstChild as IAbstractTreeNode;
                }
                else
                {
                    processor.ProcessAfterInterior(treeNode);
                    while (treeNode.NextSibling == null)
                    {
                        treeNode = treeNode.Parent as IAbstractTreeNode;
                        if (treeNode == root || treeNode == null)
                        {
                            return;
                        }
                    }
                    treeNode = treeNode.NextSibling as IAbstractTreeNode;
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
