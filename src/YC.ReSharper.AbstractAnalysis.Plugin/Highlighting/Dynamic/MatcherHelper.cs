using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Dynamic
{
    public static class MatcherHelper
    {
        public static Core.Processor YcProcessor { get; set; }

        private static Dictionary<string, string> myMatched = new Dictionary<string, string>();
        
        public static void AddMatch(string left, string right)
        {
            myMatched.Add(left, right);
        }

        public static string GetMatch(string brother)
        {
            string ycToken = YcHelper.GetYcTokenName(brother);
            if (string.IsNullOrEmpty(ycToken))
                return null;

            string pair = "";
            if (myMatched.ContainsKey(ycToken))
            {
                pair = myMatched[ycToken];
            }

            if (myMatched.ContainsValue(ycToken))
            {
                pair =  myMatched.First(pairMatched => pairMatched.Value == ycToken).Key;
            }
                
            return YcHelper.GetStringValue (pair);
        }

        public static void ReCreate()
        {
            YcHelper.ReCreate();
        }
    }
}
