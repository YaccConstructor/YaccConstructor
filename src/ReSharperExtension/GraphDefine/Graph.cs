using System.Windows.Media;
using QuickGraph;
using QuickGraph.Collections;

namespace ReSharperExtension
{

    public class Graph : BidirectionalGraph<Vertex, Edge>
    {
        public virtual bool AddTaggedEdge(TaggedEdge<int, string> e)
        {
            var sourceVertex = new Vertex(e.Source.ToString()) { ID = e.Source };
            var targetVertex = new Vertex(e.Target.ToString()) { ID = e.Target };
            this.AddVertex(sourceVertex);
            this.AddVertex(targetVertex);
            var edge = new Edge(e.Tag, sourceVertex, targetVertex, Brushes.GreenYellow) {Text = e.Tag};
            this.AddEdge(edge);
            return true;
        }

    }
}
