using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Media;

using JetBrains.Application.Threading.Tasks;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using ReSharperExtension.GraphDefine;
using ReSharperExtension.Highlighting;
using ReSharperExtension.Highlighting.Dynamic;
using ReSharperExtension.Settings;
using ReSharperExtension.YcIntegration;

using YC.SDK;

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
        private static readonly ReSharperHelper<DocumentRange, ITreeNode> YcProcessor = ReSharperHelper<DocumentRange, ITreeNode>.Instance;
        public static ArrayList DataGraphs;
        
        static Handler()
        {
            DataGraphs = new ArrayList();

            foreach (var lexEvent in YcProcessor.LexingFinished)
            {
                lexEvent.AddHandler(SaveTokens);
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
            LanguageHelper.Update(args.Lang);
            IList<HighlightingInfo> highlightings = new List<HighlightingInfo>();

            Action action =
                () => args.Tokens
                    .ForEach(node => highlightings.AddRange(ToHighlightingInfo(node as ITreeNode)));

            using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            {
                fibers.EnqueueJob(action);
            }

            Process.DoHighlighting(new DaemonStageResult(highlightings));
        }

        private static void SaveTokens<T>(object sender, CommonInterfaces.LexingFinishedArgs<T> args)
        {
            IEnumerable<ITreeNode> tokens = args.Tokens.Select(node => node as ITreeNode);
            ExistingRanges.AddRanges(tokens);
        }

        private static IList<HighlightingInfo> ToHighlightingInfo(ITreeNode node)
        {
            if (node == null)
                return new List<HighlightingInfo>();

            ICollection<DocumentRange> colorConstantRange = node.UserData.GetData(Constants.Ranges);
            IHighlighting highlighting = new TokenHighlighting(node);

            if (colorConstantRange == null)
                return new List<HighlightingInfo>();

            return colorConstantRange.Select(
                range => new HighlightingInfo(range, highlighting, new Severity?())).ToList();
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

        private static readonly Dictionary<string, int> parsedSppf = new Dictionary<string, int>();
        /// <summary>
        /// Translates sppf to ReSharper trees and stores result. It's need further.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args">Now it contains only language</param>
        private static void OnParsingFinished(object sender, CommonInterfaces.ParsingFinishedArgs args)
        {
            string lang = args.Lang;

            //if (!parsedSppf.ContainsKey(lang))
            //    parsedSppf.Add(lang, 0);
            //else
            //    parsedSppf[lang]++;

            //Action action =
            //    () =>
            //    {
            //        var isEnd = false;
            //        while (!isEnd)
            //        {
            //            Tuple<ITreeNode, bool> res = YcProcessor.GetNextTree(lang, parsedSppf[lang]);
            //            ExistingRanges.AddTree(res.Item1);
            //            isEnd = res.Item2;
            //        }
            //    };

            //using (TaskBarrier fibers = Process.DaemonProcess.CreateFibers())
            //{
            //    fibers.EnqueueJob(action);
            //}
        }

        public static void Init()
        {
            
        }

        internal static void UpdateHotspots(IEnumerable<HotspotModelView> items)
        {
            hotspots = items.Select(HotspotModelView.ToHotspot).ToList();
        }
    }
}
