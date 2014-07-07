using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using JetBrains.Application.Progress;
using JetBrains.Application.Settings;
using JetBrains.Application.Threading.Tasks;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using YC.ReSharper.AbstractAnalysis.Plugin.Core;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public abstract class MyIncrementalDaemonStageProcessBase : MyDaemonStageProcessBase
    {
        private readonly IDaemonProcess myProcess;
        private readonly DaemonProcessKind myProcessKind;
        private Action<DaemonStageResult> commiter;
        private Core.Processor ycProcessor = null;

        private Dictionary<string, int> parsedSppf = new Dictionary<string, int>();

        protected MyIncrementalDaemonStageProcessBase(IDaemonProcess process, IContextBoundSettingsStore settingsStore, DaemonProcessKind processKind)
            : base(process, settingsStore)
        {
            myProcess = process;
            mySettingsStore = settingsStore;
            myProcessKind = processKind;
        }

        public override void Execute(Action<DaemonStageResult> commiter)
        {
            try
            {
                if (myProcessKind != DaemonProcessKind.VISIBLE_DOCUMENT)
                    return;

                this.commiter = commiter;
                MatcherHelper.ClearNodeCover();
                //Action globalHighlighter = UpdateYCProcessor;

                //using (var fibers = DaemonProcess.CreateFibers())
                //{
                    //fibers.EnqueueJob(globalHighlighter);
                //}
                UpdateYCProcessor();
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

        private void UpdateYCProcessor()
        {
            // Getting PSI (AST) for the file being highlighted
            var sourceFile = myDaemonProcess.SourceFile;
            var file = sourceFile.GetPsiServices().Files.GetDominantPsiFile<CSharpLanguage>(sourceFile) as ICSharpFile;
            if (file == null)
                return;

            // Running visitor against the PSI
            ycProcessor = new YC.ReSharper.AbstractAnalysis.Plugin.Core.Processor(file);
            ycProcessor.LexingFinished += OnLexingFinished;
            ycProcessor.ParsingFinished += OnParsingFinished;
            ycProcessor.Process();
        }

        /// <summary>
        /// Do highlighting some tokens chunk.
        /// </summary>
        /// <param name="sender">Now always null</param>
        /// <param name="args"></param>
        private void OnLexingFinished(object sender, Core.LexingFinishedArgs args)
        {
            if (commiter == null)
                return;

            var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
            var processor = new ProcessorBase(this, consumer);

            string xmlPath = ycProcessor.XmlPath(args.Lang);
            string lang = ycProcessor.LangToString(args.Lang);
            ColorHelper.ParseFile(xmlPath, lang);

            using (TaskBarrier fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(
                    () =>
                    {
                        foreach (ITreeNode treeNode in args.Tokens)
                        {
                            processor.ProcessAfterInterior(treeNode);
                        }
                    }
                );
            }

            commiter(new DaemonStageResult(consumer.Highlightings) { Layer = 1 });
        }

        /// <summary>
        /// Do translate sppf to ReSharper trees. It is need further.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args">Now it contains only language</param>
        private void OnParsingFinished(object sender, ParsingFinishedArgs args)
        {
            if (ycProcessor == null)
                return;

            var lang = args.Lang;
            MatcherHelper.YcProcessor = ycProcessor;

            string strLang = ycProcessor.LangToString(lang);

            if (!parsedSppf.ContainsKey(strLang))
            {
                parsedSppf.Add(strLang, 0);
            }
            else
            {
                parsedSppf[strLang]++;
            }

            using (TaskBarrier fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(() =>
                {
                    var isEnd = false;

                    while (!isEnd)
                    {
                        Tuple<ITreeNode, bool> res = ycProcessor.GetNextTree(lang, parsedSppf[strLang]);
                        ITreeNode tree = res.Item1;
                        isEnd = res.Item2;
                        MatcherHelper.NodeCover.Add(tree);
                    }
                });
            }
        }
        #region
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
        #endregion

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
