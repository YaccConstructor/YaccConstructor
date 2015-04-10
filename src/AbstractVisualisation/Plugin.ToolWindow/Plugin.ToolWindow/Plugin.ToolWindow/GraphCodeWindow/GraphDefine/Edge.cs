using System.Windows.Media;
using GraphX;

namespace Plugin.ToolWindow
{
    /* DataEdge is the data class for the edges. It contains all custom edge data specified by the user.
     * This class also must be derived from EdgeBase class that provides properties and methods mandatory for
     * correct GraphX operations.
     * Some of the useful EdgeBase members are:
     *  - ID property that stores unique positive identfication number. Property must be filled by user.
     *  - IsSelfLoop boolean property that indicates if this edge is self looped (eg have identical Target and Source vertices) 
     *  - RoutingPoints collection of points used to create edge routing path. If Null then straight line will be used to draw edge.
     *      In most cases it is handled automatically by GraphX.
     *  - Source property that holds edge source vertex.
     *  - Target property that holds edge target vertex.
     *  - Weight property that holds optional edge weight value that can be used in some layout algorithms.
     */

    public class Edge : EdgeBase<Vertex>
    {
        /// <summary>
        /// Default constructor. We need to set at least Source and Target properties of the edge.
        /// </summary>
        /// <param name="source">Source vertex data</param>
        /// <param name="target">Target vertex data</param>
        /// <param name="weight">Optional edge weight</param>
        public Edge(object info, Vertex source, Vertex target, Brush br, double weight = 1)
            : base(source, target, weight)
        {
            thisobject = info;
            b = br;
        }
        /// <summary>
        /// Default parameterless constructor (for serialization compatibility)
        /// </summary>

        public Edge()
            : base(null, null, 1)
        {
        }
        public Brush GetBrush()
        {
            return b;
        }


        /// <summary>
        /// Custom string p
        /// roperty for example
        /// </summary>
        private Brush b;

        public object thisobject;
        public int codeline;
        private int codecolumn;
        public string Text { get; set; }

        #region GET members
        public override string ToString()
        {
            return Text;
        }
        public object thisObj()
        {
            return thisobject;
        }
        #endregion
    }
}
