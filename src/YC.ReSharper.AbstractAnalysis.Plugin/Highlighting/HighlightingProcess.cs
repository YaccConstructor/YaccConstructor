using System;
using System.Collections.Generic;
using JetBrains.Application.Settings;
using JetBrains.Application.Threading.Tasks;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public class HighlightingProcess : IDaemonStageProcess
    {
        private readonly DaemonProcessKind myProcessKind;
        private Action<DaemonStageResult> myCommiter;
        private IContextBoundSettingsStore mySettingsStore;

        public static YC.AbstractAnalysis.Helper.ReSharperHelper YCProcessor;

        public IDaemonProcess DaemonProcess { get; private set; }

        public IFile File
        {
            get
            {
                IPsiServices psiServices = DaemonProcess.SourceFile.GetPsiServices();
                return psiServices.Files.GetDominantPsiFile<CSharpLanguage>(DaemonProcess.SourceFile);
            }
        }

        public HighlightingProcess(IDaemonProcess process, IContextBoundSettingsStore settingsStore, DaemonProcessKind processKind)
        {
            if (YCProcessor == null)
            {
                CreateYCProcessor();
            }

            DaemonProcess = process;
            mySettingsStore = settingsStore;
            myProcessKind = processKind;
        }

        public void Execute(Action<DaemonStageResult> commiter)
        {
            if (myProcessKind != DaemonProcessKind.VISIBLE_DOCUMENT)
                return;

            var file = File as ICSharpFile;
            if (file == null)
                return;

            myCommiter = commiter;
            MatchingBraceContextHighlighter.ExistingTrees.Clear();

            CreateYCProcessor();
            YCProcessor.Process(file);
            // remove all old highlightings
            //if (DaemonProcess.FullRehighlightingRequired)
            //myCommiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }

        private void CreateYCProcessor()
        {
            YCProcessor = new YC.AbstractAnalysis.Helper.ReSharperHelper();
            foreach (var e in YCProcessor.LexingFinished)
            {
                e.AddHandler(OnLexingFinished);
            }
            foreach (var e in YCProcessor.ParsingFinished)
            {
                e.AddHandler(OnParsingFinished);
            }
        }

        /// <summary>
        /// Do highlighting some tokens chunk.
        /// </summary>
        /// <param name="sender">Now always null</param>
        /// <param name="args"></param>
        private void OnLexingFinished(object sender, YC.AbstractAnalysis.CommonInterfaces.LexingFinishedArgs args)
        {
            if (myCommiter == null)
                return;

            var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
            var processor = new RecursiveElementProcessor(consumer, File);

            string xmlPath = YCProcessor.XmlPath(args.Lang);
            ColorHelper.ParseFile(xmlPath, args.Lang);

            using (TaskBarrier fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(
                    () => args.Tokens.ForEach(node => processor.ProcessAfterInterior(node)));
            }

            myCommiter(new DaemonStageResult(consumer.Highlightings));
        }

        /// <summary>
        /// Do translate sppf to ReSharper trees and store result. It is need further.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args">Now it contains only language</param>
        private Dictionary<string, int> parsedSppf = new Dictionary<string, int>();
        private void OnParsingFinished(object sender, YC.AbstractAnalysis.CommonInterfaces.ParsingFinishedArgs args)
        {
            if (YCProcessor == null)
                return;

            var lang = args.Lang;

            if (!parsedSppf.ContainsKey(lang))
            {
                parsedSppf.Add(lang, 0);
            }
            else
            {
                parsedSppf[lang]++;
            }

            using (TaskBarrier fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(() =>
                {
                    var isEnd = false;

                    while (!isEnd)
                    {
                        Tuple<ITreeNode, bool> res = YCProcessor.GetNextTree(lang, parsedSppf[lang]);
                        ITreeNode tree = res.Item1;
                        isEnd = res.Item2;
                        MatchingBraceContextHighlighter.ExistingTrees.Add(tree);
                    }
                });
            }
        }
    }
}
