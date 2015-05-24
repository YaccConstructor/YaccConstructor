using GraphX;
using QuickGraph;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Plugin.ToolWindow
{
    /// <summary>
    /// This is custom GraphArea representation using custom data types.
    /// GraphArea is the visual panel component responsible for drawing visuals (vertices and edges).
    /// It is also provides many global preferences and methods that makes GraphX so customizable and user-friendly.
    /// </summary>
    public class GraphArea : GraphArea<Vertex, Edge, BidirectionalGraph<Vertex, Edge>> { }
}
