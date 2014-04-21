using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphX.GraphSharp.Algorithms.OverlapRemoval;
using YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow.GraphClasses;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow
{
    public static class InitializeGraphArea
    {
        /// <summary>
        /// Prepares graph area control for future use.
        /// </summary>
        /// <returns></returns>
        public static ReadyGraphArea Initialize(DataGraph graph)
        {
            var gg_Area = new ReadyGraphArea();

            try
            {
                /*var Rand = new Random();

                //Create data graph object
                var graph = new DataGraph();
                var vlist = new List<DataVertex>();
                for (int i = 0; i < 7; ++i)
                {
                    vlist.Add(new DataVertex() { ID = i, Text = i.ToString() });
                    graph.AddVertex(vlist[i]);
                }

                graph.AddEdge(new DataEdge(vlist[0], vlist[1], 1) {ToolTipText = "T_T_SomeText_SomeText_SomeText" });
                graph.AddEdge(new DataEdge(vlist[0], vlist[2], 2) { ToolTipText = "T_T_SomeText_SomeText_SomeText" });
                graph.AddEdge(new DataEdge(vlist[1], vlist[3], 3) {ToolTipText = "T_T_SomeText_SomeText_SomeText" });
                graph.AddEdge(new DataEdge(vlist[2], vlist[4], 4) {ToolTipText = "T_T_SomeText_SomeText_SomeText" });
                graph.AddEdge(new DataEdge(vlist[3], vlist[5], 5) {ToolTipText = "T_T_SomeText_SomeText_SomeText" });
                graph.AddEdge(new DataEdge(vlist[3], vlist[6], 6) {ToolTipText = "T_T_SomeText_SomeText_SomeText" });

                //Create and add vertices using some DataSource for ID's
                /*                for (int i = 0; i < 10; ++i)
                                    graph.AddVertex(new DataVertex() { ID = i, Text = i.ToString() });

                                var vlist = graph.Vertices.ToList();
                                //Generate random edges for the vertices
                                foreach (var item in vlist)
                                {
                                    if (Rand.Next(0, 50) > 25) continue;
                                    var vertex2 = vlist[Rand.Next(0, graph.VertexCount - 1)];
                                    graph.AddEdge(new DataEdge(item, vertex2, Rand.Next(1, 50)) { Text1 = "Edge" });
                                }
                 */
                var LogicCore = new LogicCore();

                //This property sets layout algorithm that will be used to calculate vertices positions
                //Different algorithms uses different values and some of them uses edge Weight property.
                LogicCore.DefaultLayoutAlgorithm = GraphX.LayoutAlgorithmTypeEnum.Tree;
                //Now we can set optional parameters using AlgorithmFactory
                LogicCore.DefaultLayoutAlgorithmParams =
                                   LogicCore.AlgorithmFactory.CreateLayoutParameters(GraphX.LayoutAlgorithmTypeEnum.KK);
                //Unfortunately to change algo parameters you need to specify params type which is different for every algorithm.
                ((KKLayoutParameters)LogicCore.DefaultLayoutAlgorithmParams).MaxIterations = 100;

                //This property sets vertex overlap removal algorithm.
                //Such algorithms help to arrange vertices in the layout so no one overlaps each other.
                LogicCore.DefaultOverlapRemovalAlgorithm = GraphX.OverlapRemovalAlgorithmTypeEnum.FSA;
                //Setup optional params
                LogicCore.DefaultOverlapRemovalAlgorithmParams =
                                  LogicCore.AlgorithmFactory.CreateOverlapRemovalParameters(GraphX.OverlapRemovalAlgorithmTypeEnum.FSA);
                ((OverlapRemovalParameters)LogicCore.DefaultOverlapRemovalAlgorithmParams).HorizontalGap = 50;
                ((OverlapRemovalParameters)LogicCore.DefaultOverlapRemovalAlgorithmParams).VerticalGap = 50;

                //This property sets edge routing algorithm that is used to build route paths according to algorithm logic.
                //For ex., SimpleER algorithm will try to set edge paths around vertices so no edge will intersect any vertex.
                LogicCore.DefaultEdgeRoutingAlgorithm = GraphX.EdgeRoutingAlgorithmTypeEnum.SimpleER;

                //This property sets async algorithms computation so methods like: Area.RelayoutGraph() and Area.GenerateGraph()
                //will run async with the UI thread. Completion of the specified methods can be catched by corresponding events:
                //Area.RelayoutFinished and Area.GenerateGraphFinished.
                LogicCore.AsyncAlgorithmCompute = false;

                //Finally assign logic core to GraphArea object
                gg_Area.LogicCore = LogicCore;// as IGXLogicCore<DataVertex, DataEdge, BidirectionalGraph<DataVertex, DataEdge>>;

                gg_Area.InitializeComponent();
                gg_Area.GenerateGraph(graph, true, true, true);
                gg_Area.ShowAllEdgesLabels(true);
                gg_Area.AlignAllEdgesLabels(true);
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message);
                MessageBox.Show(e.StackTrace);
            }
            return gg_Area;
        }
    }
}
