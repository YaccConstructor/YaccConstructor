using System;
using JetBrains.Annotations;
using JetBrains.Application.Progress;
using JetBrains.Application.Settings;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public abstract class MyIncrementalDaemonStageProcessBase : MyDaemonStageProcessBase
    {
        private readonly IDaemonProcess myProcess;
        private readonly DaemonProcessKind myProcessKind;
        private Action<DaemonStageResult> commiter;

        protected MyIncrementalDaemonStageProcessBase(IDaemonProcess process, IContextBoundSettingsStore settingsStore, DaemonProcessKind processKind)
            : base(process, settingsStore)
        {
            myProcess = process;
            mySettingsStore = settingsStore;
            myProcessKind = processKind;
        }

        private Core.Processor UpdateYCProcessor()
        {
            // Getting PSI (AST) for the file being highlighted
            var sourceFile = myDaemonProcess.SourceFile;
            var file = sourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) as ICSharpFile;
            if (file == null)
                return null;

            // Running visitor against the PSI
            var ycProcessor = new YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor(file);
            ycProcessor.LexingFinished += OnLexingFinished;
            ycProcessor.Process();
            
            return ycProcessor;
        }

        private void OnLexingFinished(object sender, Core.LexingFinishedArgs args)
        {
            if (commiter == null)
                return;

            var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
            var processor = new ProcessorBase(this, consumer);

            ColorHelper.ParseFile(args.Xml, args.Lang);

            foreach (ITreeNode treeNode in args.Tokens)
            {
                processor.ProcessAfterInterior(treeNode);
            }
            commiter(new DaemonStageResult(consumer.Highlightings) { Layer = 1 });
        }


        public override void Execute(Action<DaemonStageResult> commiter)
        {
            try
            {
                if (myProcessKind != DaemonProcessKind.VISIBLE_DOCUMENT)
                    return;

                this.commiter = commiter;
                Action globalHighlighter = () =>
                {
                    ProcessThisAndDescendants(commiter);
                };

                using (var fibers = DaemonProcess.CreateFibers())
                {
                    fibers.EnqueueJob(globalHighlighter);
                }

                // remove all old highlightings
                //if (DaemonProcess.FullRehighlightingRequired)
                //commiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
            }
            catch (Exception ex)
            {
                if (ex is ProcessCancelledException)
                    return;
                throw;
            }
        }

        private void ProcessThisAndDescendants(Action<DaemonStageResult> commiter)
        {
            Core.Processor ycProcessor = UpdateYCProcessor();
            if (ycProcessor == null)
                return;
            
            MatcherHelper.YcProcessor = ycProcessor;
            MatcherHelper.ClearNodeCover();

            //var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
            //var processor = new ProcessorBase(this, consumer);

            ITreeNode tree = ycProcessor.GetNextTree();
            
            while (tree != null)
            {
                //string lang = ycProcessor.CurrentLang.ToLower();
                //ColorHelper.ParseFile(ycProcessor.CurrentXmlPath, lang);
                //ProcessDescendants(tree, processor);
                
                //commiter(new DaemonStageResult(consumer.Highlightings) { Layer = 1 });
                MatcherHelper.NodeCover.Add(tree);

                tree = ycProcessor.GetNextTree();
            }
            //commiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
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
