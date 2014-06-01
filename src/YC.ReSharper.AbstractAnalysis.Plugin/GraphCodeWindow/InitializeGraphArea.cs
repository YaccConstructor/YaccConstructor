using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Input;
using GraphX;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphX.GraphSharp.Algorithms.Layout.Simple.Hierarchical;
using GraphX.GraphSharp.Algorithms.Layout.Simple.Tree;
using GraphX.GraphSharp.Algorithms.OverlapRemoval;
using GraphX.Models;
using JetBrains.ReSharper.Daemon.CSharp.Errors;
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
                logicCore.DefaultLayoutAlgorithm = GraphX.LayoutAlgorithmTypeEnum.Tree;
                logicCore.DefaultLayoutAlgorithmParams =
                                   logicCore.AlgorithmFactory.CreateLayoutParameters(GraphX.LayoutAlgorithmTypeEnum.Tree);
                ((SimpleTreeLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).LayerGap = 100;
                ((SimpleTreeLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).VertexGap = 100;
                ((SimpleTreeLayoutParameters) logicCore.DefaultLayoutAlgorithmParams).WidthPerHeight = 1000;
                ((SimpleTreeLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).OptimizeWidthAndHeight = true;

                /************************************/

                /*logicCore.DefaultLayoutAlgorithm = GraphX.LayoutAlgorithmTypeEnum.KK;
                logicCore.DefaultLayoutAlgorithmParams =
                                   logicCore.AlgorithmFactory.CreateLayoutParameters(GraphX.LayoutAlgorithmTypeEnum.KK);
                ((KKLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).MaxIterations = 100;*/

                /*************************************/



                logicCore.DefaultOverlapRemovalAlgorithm = GraphX.OverlapRemovalAlgorithmTypeEnum.FSA;
                logicCore.DefaultOverlapRemovalAlgorithmParams =
                                  logicCore.AlgorithmFactory.CreateOverlapRemovalParameters(GraphX.OverlapRemovalAlgorithmTypeEnum.FSA);
                ((OverlapRemovalParameters)logicCore.DefaultOverlapRemovalAlgorithmParams).HorizontalGap = 100;
                ((OverlapRemovalParameters)logicCore.DefaultOverlapRemovalAlgorithmParams).VerticalGap = 100;

                logicCore.DefaultEdgeRoutingAlgorithm = GraphX.EdgeRoutingAlgorithmTypeEnum.SimpleER;
                logicCore.EnableParallelEdges = true;
                logicCore.EdgeShowSelfLooped = true;
                logicCore.EdgeSelfLoopCircleOffset = new Point(0,0);
                logicCore.AsyncAlgorithmCompute = false;
                logicCore.ParallelEdgeDistance = 50;
                logicCore.EdgeSelfLoopCircleRadius = 50;
                logicCore.EdgeCurvingEnabled = true;               
                gg_Area.LogicCore = logicCore;
                gg_Area.EdgeDoubleClick += delegate(object sender, EdgeSelectedEventArgs args)
                {
                    var backRef = ((DataEdge) (args.EdgeControl.Edge)).BackRef;
                    GoToCodeEventHandler.ClickHandler(backRef, args);
                };
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
