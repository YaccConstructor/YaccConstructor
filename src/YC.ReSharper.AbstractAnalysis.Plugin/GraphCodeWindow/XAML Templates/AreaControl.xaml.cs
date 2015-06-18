using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Daemon.SolutionAnalysis.IssuesSerializer;
using JetBrains.ReSharper.Psi.Tree;
using YC.SDK;
using YC.SDK.ReSharper;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using GraphX;
using GraphX.Controls;
using GraphX.GraphSharp.Algorithms.Layout.Simple.FDP;
using GraphX.Models;
using JetBrains.TextControl;

namespace YC.ReSharper.AbstractAnalysis.Plugin
{
    /// <summary>
    /// Логика взаимодействия для AreaControl.xaml
    /// </summary>
    public partial class AreaControl : TabControl
    {
         
        //public static Helper.ReSharperHelper<DocumentRange, ITreeNode> YcProcessor = Helper.ReSharperHelper<DocumentRange, ITreeNode>.Instance;
        ////public ArrayList DataGraphs; 
        //public void Handler()
        //{
        //    foreach (var lexEvent in YcProcessor.LexingFinished)
        //        lexEvent.AddHandler(OnLexingFinished);
        //}
        //private void OnLexingFinished(object sender, CommonInterfaces.LexingFinishedArgs<ITreeNode> args)
        //{
        //    //Graph dataGraph = new Graph();
        //    //foreach (var vertex in args.Graph.Vertices)
        //    //{
        //    //    dataGraph.AddVertex(new Vertex("") {ID = vertex});
        //    //}
        //    //var vlist = dataGraph.Vertices.ToList();
        //    //foreach (var tedge in args.Graph.Edges)
        //    //{
        //    //    var sourceVertex = new Vertex(tedge.Source.ToString()) { ID = tedge.Source };
        //    //    var targetVertex = new Vertex(tedge.Target.ToString()) { ID = tedge.Target };
        //    //    int s = vlist.IndexOf(sourceVertex);
        //    //    int t = vlist.IndexOf(targetVertex);
        //    //    var edge = new Edge(tedge.Tag, vlist[s], vlist[t], Brushes.Black) { Text = tedge.Tag };
        //    //    dataGraph.AddEdge(edge);
        //    //}
        //    //DataGraphs.Add(dataGraph);
            
        //    //BackgroundWorker bw = new BackgroundWorker();
        //    ////System.ComponentModel.ProgressChangedEventHandler)
        //    ////bw.ProgressChanged += (System.ComponentModel.ProgressChangedEventHandler)WindowAction.AddTabControls;
        //    //bw.RunWorkerAsync();
        //    //bw.DoWork += (System.ComponentModel.DoWorkEventHandler)WindowAction.AddTabControls;
        //    //bw.RunWorkerAsync();
        //    ////TabItem tab = new TabItem();
        //    //EControl.Items.Add(new TabItem());
        //}
        #region CreateGraph
        public AreaControl()
        {
            InitializeComponent();
            //Handler();
            //ZoomControl.SetViewFinderVisibility(zoomctrler, Visibility.Visible);
            //Set Fill zooming strategy so whole graph will be always visible
            //zoomctrler.ZoomToFill();
            //DataGraphs = new ArrayList();

        }
        public void GraphArea_Setup()
        {
            //WindowAction.arcont.Area.SetVerticesMathShape(VertexShape.Circle);
            //Lets create logic core and filled data graph with edges and vertices
            var logicCore = new LogicCore() {  };
            //This property sets layout algorithm that will be used to calculate vertices positions
            //Different algorithms uses different values and some of them uses edge Weight property.
            logicCore.DefaultLayoutAlgorithm = LayoutAlgorithmTypeEnum.Tree;
            
            //Now we can set parameters for selected algorithm using AlgorithmFactory property. This property provides methods for
            //creating all available algorithms and algo parameters.
            logicCore.DefaultLayoutAlgorithmParams = logicCore.AlgorithmFactory.CreateLayoutParameters(LayoutAlgorithmTypeEnum.LinLog);
            //Unfortunately to change algo parameters you need to specify params type which is different for every algorithm.
            ((LinLogLayoutParameters)logicCore.DefaultLayoutAlgorithmParams).IterationCount = 100;

            //This property sets vertex overlap removal algorithm.
            //Such algorithms help to arrange vertices in the layout so no one overlaps each other.
            logicCore.DefaultOverlapRemovalAlgorithm = OverlapRemovalAlgorithmTypeEnum.FSA;
            //Default parameters are created automaticaly when new default algorithm is set and previous params were NULL
            logicCore.DefaultOverlapRemovalAlgorithmParams.HorizontalGap = 50;
            logicCore.DefaultOverlapRemovalAlgorithmParams.VerticalGap = 50;
            //This property sets edge routing algorithm that is used to build route paths according to algorithm logic.
            //For ex., SimpleER algorithm will try to set edge paths around vertices so no edge will intersect any vertex.
            //Bundling algorithm will try to tie different edges that follows same direction to a single channel making complex graphs more appealing.
            logicCore.DefaultEdgeRoutingAlgorithm = EdgeRoutingAlgorithmTypeEnum.SimpleER;

            //This property sets async algorithms computation so methods like: Area.RelayoutGraph() and Area.GenerateGraph()
            //will run async with the UI thread. Completion of the specified methods can be catched by corresponding events:
            //Area.RelayoutFinished and Area.GenerateGraphFinished.
            logicCore.AsyncAlgorithmCompute = false;

            //Finally assign logic core to GraphArea object
            //Area.LogicCore = logicCore;// as IGXLogicCore<DataVertex, DataEdge, BidirectionalGraph<DataVertex, DataEdge>>;
            //Area.MoveAnimation = AnimationFactory.CreateMoveAnimation(MoveAnimation.Move, TimeSpan.FromSeconds(1));
            
        }
        public Graph Graph_Setup()
        {
            Graph dataGraph = new Graph();
            for (int i = 1; i < 10; i++)
            {
                var dataVertex = new Vertex("MyVertex " + i) { ID = i };
                dataGraph.AddVertex(dataVertex);
            }
            var vlist = dataGraph.Vertices.ToList();
            int[] a = new int[] { 0 };
            var dataEdge = new Edge(a, vlist[0], vlist[1], Brushes.GreenYellow) { Text = string.Format("{0} -> {1}", vlist[0], vlist[1]) };
            dataGraph.AddEdge(dataEdge);
            dataEdge = new Edge(a, vlist[2], vlist[3], Brushes.Red) { Text = string.Format("{0} -> {1}", vlist[2], vlist[3]) };
            dataGraph.AddEdge(dataEdge);
            //WindowAction.arcont.Area.SetVerticesMathShape(VertexShape.Circle);
            return dataGraph;
        }

        public void MainWindow_Loaded(object sender, RoutedEventArgs e)
        {
            gg_but_randomgraph_Click(null, null);
        }

        public void gg_but_relayout_Click(object sender, RoutedEventArgs e)
        {
            //Area.RelayoutGraph();
            //zoomctrler.ZoomToFill();
        }

        public void gg_but_randomgraph_Click(object sender, RoutedEventArgs e)
        {
            foreach (TabItem areas in EControl.Items)
            {
                ZoomControl z = (ZoomControl)areas.Content;

                EmptyGraphArea area = (EmptyGraphArea)z.Content;
                if (area != null)
                {
                    area.GenerateGraph(true, true);
                    foreach (var d in area.EdgesList)
                    {
                        d.Value.Foreground = d.Key.GetBrush();
                    }
                    area.EdgeDoubleClick += Area_EdgeDoubleClick;
                    area.EdgeMouseMove += EdgeMouseMove;
                    area.EdgeMouseLeave += EdgeMouseUnMove;
                    area.SetEdgesDashStyle(EdgeDashStyle.Dash);
                    area.ShowAllEdgesArrows(true);
                    area.SetVerticesMathShape(VertexShape.Circle);
                    area.ShowAllEdgesLabels(true);
                }
            }
            //Area.GenerateGraph(true, true);
            //foreach (var d in Area.EdgesList)
            //{
            //    d.Value.Foreground = d.Key.GetBrush();
            //}
            //Area.EdgeDoubleClick += Area_EdgeDoubleClick;
            //Area.EdgeMouseMove += EdgeMouseMove;
            //Area.EdgeMouseLeave += EdgeMouseUnMove;
            ///* 
            // * After graph generation is finished you can apply some additional settings for newly created visual vertex and edge controls
            // * (VertexControl and EdgeControl classes).
            // * 
            // */

            ////This method sets the dash style for edges. It is applied to all edges in Area.EdgesList. You can also set dash property for
            ////each edge individually using EdgeControl.DashStyle property.
            ////For ex.: Area.EdgesList[0].DashStyle = GraphX.EdgeDashStyle.Dash;
            //Area.SetEdgesDashStyle(EdgeDashStyle.Dash);
            //Area.ShowAllEdgesArrows(true);
            //Area.SetVerticesMathShape(VertexShape.Circle);
            //Area.ShowAllEdgesLabels(true);

            //zoomctrler.ZoomToFill();
        }
        #endregion
        #region EdgeHandlers
        public void EdgeMouseMove(object sender, EdgeSelectedEventArgs e)
        {
            e.EdgeControl.Foreground = Brushes.BlueViolet;
        }
        public void EdgeMouseUnMove(object sender, EdgeSelectedEventArgs e)
        {
            Edge p = (Edge)e.EdgeControl.Edge;

            e.EdgeControl.Foreground = p.GetBrush();
        }
        public void Area_EdgeDoubleClick(object sender, EdgeSelectedEventArgs e)
        {
            Edge p = (Edge)e.EdgeControl.Edge;
            MessageBox.Show("event was handled by vertex: " + e.EdgeControl.Edge.ToString() + p.thisObj().ToString());
            WindowAction.GoToCode(WindowAction.textControl, (Edge)e.EdgeControl.Edge);
        }
        public static void GoToCode(ITextControl t, Edge e)
        {
            var textControl = t;
            textControl.Caret.MoveTo(e.codeline, new CaretVisualPlacement());
        }
        #endregion
        public void Connect(int connectionId, object target)
        {
            throw new NotImplementedException();
        }
    }
}
