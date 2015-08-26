using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Media;
using JetBrains.Application.Threading.Tasks;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi.Tree;
using ReSharperExtension.Highlighting;
using ReSharperExtension.Highlighting.Dynamic;
using ReSharperExtension.YcIntegration;
using YC.SDK;
using ReSharperExtension.Settings;

namespace ReSharperExtension
{
    static class Handler
    {
        private static List<Hotspot.Hotspot> hotspots;

        public static List<Hotspot.Hotspot> Hotspots
        {
            get
            {
                if (hotspots == null)
                {
                    UpdateHotspots(ConfigurationManager.LoadHotspotData());
                }
                return hotspots;
            } 
        }

        public static HighlightingProcess Process { get; set; }
        private static ReSharperHelper<DocumentRange, ITreeNode> YcProcessor = ReSharperHelper<DocumentRange, ITreeNode>.Instance;
        public static ArrayList DataGraphs;
        
        static Handler()
        {
            DataGraphs = new ArrayList();

            foreach (var lexEvent in YcProcessor.LexingFinished)
            {
                lexEvent.AddHandler(HighlightTokens);
                lexEvent.AddHandler(UpdateDataGraph);
            }

            foreach (var parseEvent in YcProcessor.ParsingFinished)
                parseEvent.AddHandler(OnParsingFinished);
        }

        /// <summary>
        /// Highlights all tokens that are passed with parameter args.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="sender">Now it's always null</param>
        /// <param name="args">Contains info about tokens and language</param>
        private static void HighlightTokens<T>(object sender, CommonInterfaces.LexingFinishedArgs<T> args)
        {
            IHighlightingConsumer consumer = Process.Consumer;
            var processor = new TreeNodeProcessor(consumer, Process.File);

            string xmlPath = YcProcessor.GetXmlPath(args.Lang);
            ColorHelper.ParseFile(xmlPath, args.Lang);

            Action action =
                () => args.Tokens.ForEach(node => 
                    processor.ProcessAfterInterior(node as ITreeNode));

            using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }

            Process.DoHighlighting(new DaemonStageResult(consumer.Highlightings));
        }

        /// <summary>
        /// Updates info about existing graphs that exist in current file
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="sender"></param>
        /// <param name="args">Contains info about graph</param>
        private static void UpdateDataGraph<T>(object sender, CommonInterfaces.LexingFinishedArgs<T> args)
        {
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
        }

        /// <summary>
        /// Translates sppf to ReSharper trees and stores result. It's need further.
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

        internal static void UpdateHotspots(IEnumerable<HotspotModelView> items)
        {
            var hotspotItems = items.Select(HotspotModelView.ToHotspot);
            hotspots = new List<Hotspot.Hotspot>(hotspotItems);
        }
    }
}
