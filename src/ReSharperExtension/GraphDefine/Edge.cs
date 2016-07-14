using System.Windows.Media;
using GraphX.PCL.Common.Models;

namespace ReSharperExtension.GraphDefine
{
    public class Edge : EdgeBase<Vertex>
    {
        /// <summary>
        /// Default constructor. We need to set at least Source and Target properties of the edge.
        /// </summary>
        /// <param name="source">Source vertex data</param>
        /// <param name="target">Target vertex data</param>
        /// <param name="weight">Optional edge weight</param>
        public Edge(object info, Vertex source, Vertex target, SolidColorBrush br = null, double weight = 1)
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
        public SolidColorBrush GetBrush()
        {
            return b;
        }


        /// <summary>
        /// Custom string p
        /// roperty for example
        /// </summary>
        private SolidColorBrush b;

        public object thisobject;
        public int codeline;
        //private int codecolumn;
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
