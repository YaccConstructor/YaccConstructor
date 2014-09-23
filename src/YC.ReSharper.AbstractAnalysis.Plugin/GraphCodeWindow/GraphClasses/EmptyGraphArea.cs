using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using GraphX;
using QuickGraph;

namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow.GraphClasses
{
    /// <summary>
    /// Layout visual class. It is base for XAML template.
    /// </summary>
    public class EmptyGraphArea : GraphArea<DataVertex, DataEdge, BidirectionalGraph<DataVertex, DataEdge>> { }
}
