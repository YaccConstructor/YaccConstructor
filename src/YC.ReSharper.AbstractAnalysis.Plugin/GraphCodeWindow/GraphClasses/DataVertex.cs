using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using GraphX;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow.GraphClasses
{
    /// <summary>
    /// Vertex data object
    /// </summary>
    public class DataVertex : VertexBase
    {
        public string Text { get; set; }

        public override string ToString()
        {
            return Text;
        }
    }
}
