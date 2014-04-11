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
        private readonly IContextBoundSettingsStore mySettingsStore;
        private readonly DaemonProcessKind myProcessKind;

        protected MyIncrementalDaemonStageProcessBase(IDaemonProcess process, IContextBoundSettingsStore settingsStore, DaemonProcessKind processKind)
            : base(process, settingsStore)
        {
            myProcess = process;
            mySettingsStore = settingsStore;
            myProcessKind = processKind;
        }

        private void UpdateForest()
        {
            var sourceFile = myProcess.SourceFile;
            var file = sourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) as ICSharpFile;
            if (file == null)
                return;

            // Running visitor against the PSI
            var processor = new YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor(file);
            processor.Process();
            var fSharpforest = processor.TreeNode;

            if (fSharpforest == null)
            {
                return;
                throw new Exception("TreeNode is null!");
            }

            var forest = new List<IAbstractTreeNode>();
            foreach (IEnumerable<IAbstractTreeNode> obj in fSharpforest)
            {
                forest.AddRange(obj);
            }

            TreeNodeHolder.Forest = forest;
            TreeNodeHolder.ParseFile("CalcHighlighting.xml");

            // Checking if the daemon is interrupted by user activity
            //if (myProcess.InterruptFlag)
                //throw new ProcessCancelledException();
        }


        public override void Execute(Action<DaemonStageResult> commiter)
        {
            if (myProcessKind != DaemonProcessKind.VISIBLE_DOCUMENT)
                return;

            UpdateForest();

            Action globalHighlighter = () =>
            {
                var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
                ProcessThisAndDescendants(File, new ProcessorBase(this, consumer));
                commiter(new DaemonStageResult(consumer.Highlightings) {Layer = 1});
            };
            
            using (var fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(globalHighlighter);
            }

            // remove all old highlightings
            commiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }

        private void ProcessThisAndDescendants(IFile file, IRecursiveElementProcessor processor)
        {
            if (TreeNodeHolder.Forest == null)
                return;

            foreach (IAbstractTreeNode tree in TreeNodeHolder.Forest)
            {
                ProcessDescendants(tree, processor);
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
