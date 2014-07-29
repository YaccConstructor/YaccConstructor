using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Application.Settings;
using JetBrains.Application.Threading.Tasks;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using JetBrains.ReSharper.Psi.Files;
using JetBrains.ReSharper.Psi.Tree;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic;
using YC.AbstractAnalysis;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    public class HighlightingProcess : IDaemonStageProcess
    {
        private Action<DaemonStageResult> myCommiter;
        private IContextBoundSettingsStore mySettingsStore;

        private static Helper.ReSharperHelper YcProcessor = Helper.ReSharperHelper.Instance;

        public IDaemonProcess DaemonProcess { get; private set; }

        private ICSharpFile csFile;

        public HighlightingProcess(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            DaemonProcess = process;
            mySettingsStore = settingsStore;
            csFile = GetCSFile();
            SubscribeYc();
            //DocumentManager.Instance.AnalyzeDocument(DaemonProcess.Document);
        }

        private ICSharpFile GetCSFile()
        {
            IPsiServices psiServices = DaemonProcess.SourceFile.GetPsiServices();
            return psiServices.Files.GetDominantPsiFile<CSharpLanguage>(DaemonProcess.SourceFile) as ICSharpFile;
        }

        public void Update(IDaemonProcess process, IContextBoundSettingsStore settingsStore)
        {
            DaemonProcess = process;
            mySettingsStore = settingsStore;
        }

        public void Execute(Action<DaemonStageResult> commiter)
        {
            if (csFile == null)
                return;

            myCommiter = commiter;
            ExistingTreeNodes.ClearExistingTree(DaemonProcess.Document);
            var errors = YcProcessor.Process(csFile);
            OnErrors(errors);
            // remove all old highlightings
            //if (DaemonProcess.FullRehighlightingRequired)
            //myCommiter(new DaemonStageResult(EmptyArray<HighlightingInfo>.Instance));
        }

        private void OnErrors(Tuple<List<Tuple<string, DocumentRange>>, List<Tuple<string, DocumentRange>>> errors)
        {
            var highlightings = (from e in errors.Item2 select new HighlightingInfo(e.Item2, new ErrorWarning("Syntax error. Unexpected token " + e.Item1))).Concat(
                                from e in errors.Item1 select new HighlightingInfo(e.Item2, new ErrorWarning("Unexpected symbol: " + e.Item1 + ".")));
            myCommiter(new DaemonStageResult(highlightings.ToArray()));
        }

        private void SubscribeYc()
        {
            foreach (var e in YcProcessor.LexingFinished)
                e.AddHandler(OnLexingFinished);
         
            foreach (var e in YcProcessor.ParsingFinished)
                e.AddHandler(OnParsingFinished);
        }

        /// <summary>
        /// Do highlighting some tokens chunk.
        /// </summary>
        /// <param name="sender">Now always null</param>
        /// <param name="args"></param>
        private void OnLexingFinished(object sender, CommonInterfaces.LexingFinishedArgs<ITreeNode> args)
        {
            if (myCommiter == null)
                return;

            var consumer = new DefaultHighlightingConsumer(this, mySettingsStore);
            var processor = new TreeNodeProcessor(consumer, csFile);

            string xmlPath = YcProcessor.XmlPath(args.Lang);
            ColorHelper.ParseFile(xmlPath, args.Lang);

            Action action = 
                () => args.Tokens.ForEach(node => processor.ProcessAfterInterior(node));
            
            using (TaskBarrier fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }

            myCommiter(new DaemonStageResult(consumer.Highlightings));
        }

        /// <summary>
        /// Do translate sppf to ReSharper trees and store result. It is need further.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args">Now it contains only language</param>
        private Dictionary<string, int> parsedSppf = new Dictionary<string, int>();
        private void OnParsingFinished(object sender, CommonInterfaces.ParsingFinishedArgs args)
        {
            if (YcProcessor == null)
                return;

            string lang = args.Lang;

            if (!parsedSppf.ContainsKey(lang))
                parsedSppf.Add(lang, 0);
            else
                parsedSppf[lang]++;

            Action action = 
                () =>
                {
                    var isEnd = false;
                    while (!isEnd)
                    {
                        Tuple<ITreeNode, bool> res = YcProcessor.GetNextTree(lang, parsedSppf[lang]);
                        ExistingTreeNodes.AddTree(res.Item1);
                        isEnd = res.Item2;
                    }
                };

            using (TaskBarrier fibers = DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }
        }
    }
}
