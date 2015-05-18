using System.Windows.Media;
using GraphX;

namespace Plugin.ToolWindow
{
    /* DataVertex is the data class for the vertices. It contains all custom vertex data specified by the user.
     * This class also must be derived from VertexBase that provides properties and methods mandatory for
     * correct GraphX operations.
     * Some of the useful VertexBase members are:
     *  - ID property that stores unique positive identfication number. Property must be filled by user.
     *  
     */

    public class Vertex : VertexBase
    {
        /// <summary>
        /// Some string property for example purposes
        /// </summary>
        public string Text { get; set; }
        private Brush b;
        private object thisobject;

        #region Calculated or static props

        public override string ToString()
        {
            return Text;
        }

        #endregion

        /// <summary>
        /// Default parameterless constructor for this class
        /// (required for YAXLib serialization)
        /// </summary>
        public Vertex()
            : this("")
        {
        }
        public Vertex(string text = "")
        {
            Text = text;
        }
        
    }
}
