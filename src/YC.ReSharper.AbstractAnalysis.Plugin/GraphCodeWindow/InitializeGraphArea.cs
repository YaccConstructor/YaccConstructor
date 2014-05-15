using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphX.GraphSharp.Algorithms.Layout.Simple.Hierarchical;
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
                var logicCore = new LogicCore();
                //This property sets layout algorithm that will be used to calculate vertices positions
                //Different algorithms uses different values and some of them uses edge Weight property.
                logicCore.DefaultLayoutAlgorithm = GraphX.LayoutAlgorithmTypeEnum.EfficientSugiyama;
                //Now we can set optional parameters using AlgorithmFactory
                logicCore.DefaultLayoutAlgorithmParams =
                                   logicCore.AlgorithmFactory.CreateLayoutParameters(GraphX.LayoutAlgorithmTypeEnum.EfficientSugiyama);
                ((EfficientSugiyamaLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).EdgeRouting = SugiyamaEdgeRoutings.Orthogonal;
                //Unfortunately to change algo parameters you need to specify params type which is different for every algorithm.
               // ((KKLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).MaxIterations = 100;

                //This property sets vertex overlap removal algorithm.
                //Such algorithms help to arrange vertices in the layout so no one overlaps each other.
                logicCore.DefaultOverlapRemovalAlgorithm = GraphX.OverlapRemovalAlgorithmTypeEnum.FSA;
                //Setup optional params
                logicCore.DefaultOverlapRemovalAlgorithmParams =
                                  logicCore.AlgorithmFactory.CreateOverlapRemovalParameters(GraphX.OverlapRemovalAlgorithmTypeEnum.FSA);
                ((OverlapRemovalParameters)logicCore.DefaultOverlapRemovalAlgorithmParams).HorizontalGap = 100;
                ((OverlapRemovalParameters)logicCore.DefaultOverlapRemovalAlgorithmParams).VerticalGap = 100;

                //This property sets edge routing algorithm that is used to build route paths according to algorithm logic.
                //For ex., SimpleER algorithm will try to set edge paths around vertices so no edge will intersect any vertex.
                logicCore.DefaultEdgeRoutingAlgorithm = GraphX.EdgeRoutingAlgorithmTypeEnum.SimpleER;
                //This property sets async algorithms computation so methods like: Area.RelayoutGraph() and Area.GenerateGraph()
                //will run async with the UI thread. Completion of the specified methods can be catched by corresponding events:
                //Area.RelayoutFinished and Area.GenerateGraphFinished.
                logicCore.EnableParallelEdges = true;
                logicCore.EdgeShowSelfLooped = true;
                logicCore.AsyncAlgorithmCompute = false;
                logicCore.ParallelEdgeDistance = 50;
                logicCore.EdgeSelfLoopCircleRadius = 50;
                //Finally assign logic core to GraphArea object
                gg_Area.LogicCore = logicCore;// as IGXLogicCore<DataVertex, DataEdge, BidirectionalGraph<DataVertex, DataEdge>>;

                gg_Area.InitializeComponent();
                gg_Area.GenerateGraph(graph, true, true, true);
                gg_Area.ShowAllEdgesLabels(true);
                gg_Area.AlignAllEdgesLabels(false);
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
