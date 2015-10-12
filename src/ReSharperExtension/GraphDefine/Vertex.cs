using System.Windows.Media;
using GraphX;


namespace ReSharperExtension
{
    public class Vertex : VertexBase
    {
        /// <summary>
        /// Some string property for example purposes
        /// </summary>
        public string Text { get; set; }

        //private Brush b;
        //private object thisobj;
        

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

        public override bool Equals(object obj)
        {
            if (object.ReferenceEquals(this, obj))
                return true;
            if (this.GetType() != obj.GetType())
                return false;

            return this.Equals(obj as Vertex);
        }

        public virtual bool Equals(Vertex obj)
        {
            if (obj.ID == this.ID)
                return true;
            return false;
            //return base.Equals(obj);
        }

        public override int GetHashCode()
        {
            return this.ID;
        }
    }
}
