using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using FSharpx;
using JetBrains.Annotations;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow.GraphClasses;
using YC.ReSharper.AbstractAnalysis.Plugin.Core;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow
{
    /// <summary>
    /// Loads graph from Plugin.Core
    /// </summary>
    public class GraphLoader
    {
        public GraphLoader()
        {
            LoadGraphFromCoreEvent += GetProcessor;
        }

        public DataGraph Load()
        {
            var graphs = _processor.Graphs();
            var readyGraph = new DataGraph();
            if (graphs == null)
                return readyGraph;
            var initialGraph = graphs[0].Item2;
            var vertices = new List<DataVertex>();
            foreach (var v in initialGraph.Vertices)
            {
                vertices.Add(new DataVertex() {ID = v, Text = v.ToString()});
                readyGraph.AddVertex(vertices[vertices.Count - 1]);
            }
            foreach (var e in initialGraph.Edges)
            {
                var text = "";
                try
                {
                    text = e.Label.Value;
                }
                catch (Exception ex){} // if e.Label = None then text = ""
                readyGraph.AddEdge(new DataEdge(vertices[e.Source], vertices[e.Target], 1) {ToolTipText = text, BackRef = e.BackRef});
            }
            return readyGraph;
        }

        public static event EventHandler<LoadGraphEventArgs> LoadGraphFromCoreEvent;

        /// <summary>
        /// Invokes event for load initial graph
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="eventArgs"></param>
        public static void OnEvent(object sender, LoadGraphEventArgs eventArgs)
        {
            LoadGraphFromCoreEvent(sender, eventArgs);
        }

        private void GetProcessor(object sender, LoadGraphEventArgs eventArgs)
        {
            _processor = eventArgs.Processor;
        }

        private Processor _processor;
    }

    public class LoadGraphEventArgs : EventArgs
    {
        public LoadGraphEventArgs(Processor processor)
        {
            this.Processor = processor;
        }

        public Processor Processor { get; private set; }
    }
}
