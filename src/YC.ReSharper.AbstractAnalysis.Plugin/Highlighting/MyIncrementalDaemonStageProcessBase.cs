using System;
using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;
using JetBrains.Annotations;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Impl;
using JetBrains.ReSharper.Psi.Impl.Shared.InjectedPsi;
using JetBrains.ReSharper.Psi.Tree;

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
            TreeNodeHolder.ClearForest();
            UpdateYCProcessor();
            TreeNodeHolder.YcProcessor = ycProcessor;

            var tree = ycProcessor.GetNextTree();

            // fsTree is List<ITreeNode>. It can be null.
            while (tree != null)
            {
                ProcessDescendants(tree, processor);
                TreeNodeHolder.Forest.Add(tree);
                tree = ycProcessor.GetNextTree();
            }

            //var fsTree = ycProcessor.GetForestWithToken<IEnumerable<IMyTreeNode>>();
            //if (fsTree != null)
            //{
            //    foreach (IEnumerable<IMyTreeNode> nodes in fsTree)
            //    {
            //        var tree = (nodes.ToList())[0];
            //        ProcessDescendants(tree, processor);
            //    }
            //}
        }

        private void ProcessDescendants([NotNull] ITreeNode root, IRecursiveElementProcessor processor)
        {
            var treeNode = root;
            
            while (!processor.ProcessingIsFinished)
            {
                if (treeNode.FirstChild != null)
                {
                    treeNode = treeNode.FirstChild;
                }
                else
                {
                    processor.ProcessAfterInterior(treeNode);
                    while (treeNode.NextSibling == null)
                    {
                        treeNode = treeNode.Parent;
                        if (treeNode == root || treeNode == null)
                        {
                            return;
                        }
                    }
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
