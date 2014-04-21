using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using GraphX;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using Microsoft.FSharp.Core;


namespace YC.ReSharper.AbstractAnalysis.Plugin.GraphCodeWindow.GraphClasses
{
    /// <summary>
    /// Edge data object
    /// </summary>
    public class DataEdge : EdgeBase<DataVertex>
    {
        public DataEdge(DataVertex source, DataVertex target, double weight = 1)
            : base(source, target, weight)
        {
        }

        public DataEdge()
            : base(null, null, 1)
        {
        }

        public FSharpOption<ICSharpLiteralExpression> BackRef;
        public string ToolTipText { get; set; }
        public override string ToString()
        {
            return ToolTipText;
        }
    }
}
