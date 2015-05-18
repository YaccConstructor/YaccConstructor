using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Media;
using JetBrains.Application.Threading.Tasks;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon;
using JetBrains.ReSharper.Daemon.Stages;
using JetBrains.ReSharper.Psi.Tree;
using YC.SDK;
using YC.SDK.ReSharper;
using YC.ReSharper.AbstractAnalysis.Plugin.Highlighting.Dynamic;
using JetBrains.ReSharper.Psi.CSharp.Tree;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    static class Handler
    {
        public static HighlightingProcess Process { get; set; }
        private static Helper.ReSharperHelper<DocumentRange, ITreeNode> YcProcessor = Helper.ReSharperHelper<DocumentRange, ITreeNode>.Instance;
        public static ArrayList DataGraphs;
        static Handler()
        {
            DataGraphs = new ArrayList();

            foreach (var lexEvent in YcProcessor.LexingFinished)
                lexEvent.AddHandler(OnLexingFinished);

            foreach (var parseEvent in YcProcessor.ParsingFinished)
                parseEvent.AddHandler(OnParsingFinished);
        }

        /// <summary>
        /// Do highlighting some tokens chunk.
        /// </summary>
        /// <param name="sender">Now always null</param>
        /// <param name="args"></param>
        private static void OnLexingFinished(object sender, CommonInterfaces.LexingFinishedArgs<ITreeNode> args)
        {
            IHighlightingConsumer consumer = Process.Consumer;
            var processor = new TreeNodeProcessor(consumer, Process.CSharpFile);

            string xmlPath = YcProcessor.XmlPath(args.Lang);
            ColorHelper.ParseFile(xmlPath, args.Lang);

            Action action =
                () => args.Tokens.ForEach(node => processor.ProcessAfterInterior(node));

            using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }

            //
            Graph dataGraph = new Graph();
            foreach (var vertex in args.Graph.Vertices)
            {
                dataGraph.AddVertex(new Vertex("") { ID = vertex });
            }
            var vlist = dataGraph.Vertices.ToList();
            foreach (var tedge in args.Graph.Edges)
            {
                var sourceVertex = new Vertex(tedge.Source.ToString()) { ID = tedge.Source };
                var targetVertex = new Vertex(tedge.Target.ToString()) { ID = tedge.Target };
                int s = vlist.IndexOf(sourceVertex);
                int t = vlist.IndexOf(targetVertex);
                var edge = new Edge(tedge.Tag, vlist[s], vlist[t], Brushes.Black) { Text = tedge.Tag };
                dataGraph.AddEdge(edge);
            }
            DataGraphs.Add(dataGraph);
            Process.DoHighlighting(new DaemonStageResult(consumer.Highlightings));
        }

        /// <summary>
        /// Do translate sppf to ReSharper trees and store result. It is need further.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args">Now it contains only language</param>
        private static Dictionary<string, int> parsedSppf = new Dictionary<string, int>();
        private static void OnParsingFinished(object sender, CommonInterfaces.ParsingFinishedArgs args)
        {
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

            using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }
        }

        public static void Init()
        {
            
        }
    }
}
