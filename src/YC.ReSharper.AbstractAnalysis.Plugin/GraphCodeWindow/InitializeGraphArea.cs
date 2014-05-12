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
