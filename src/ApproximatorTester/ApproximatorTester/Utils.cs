using System;
using System.Linq;

namespace ApproximatorTester
{
    public class Utils
    {
        public static string GenericCfgStructureToDot(GenericCFG.GenericCFG cfg)
        {
            var edgesList = cfg.Graph.Edges
                .Select(edge => edge.Source.Id.ToString() + " -> " + edge.Target.Id.ToString())
                .ToList();
            return String.Join("\n", edgesList);
        }
    }
}