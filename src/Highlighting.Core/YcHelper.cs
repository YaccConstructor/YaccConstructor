using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.ReSharper.Daemon.CSharp.Errors;

namespace Highlighting.Core
{
    public static class YcHelper
    {
        private static List<string> ununique = new List<string>();
        private static Dictionary<string, string> ycTokenToString = new Dictionary<string, string>();

        public static void AddYcItem(string key, string value)
        {
            if (String.IsNullOrEmpty(key) ||
                String.IsNullOrEmpty(value) ||
                ununique.Contains(key))
                return;

            if (ycTokenToString.ContainsKey(key))
            {
                if (ycTokenToString[key] != value)
                {
                    ycTokenToString.Remove(key);
                    ununique.Add(key);
                }
            }
            else
            {
                ycTokenToString.Add(key, value);
            }
        }

        /// <summary>
        /// Returns ycToken from string value.
        /// For example if str is "(" then method returns LBRACE
        /// </summary>
        /// <param name="ycToken"></param>
        /// <returns></returns>
        public static string GetYcTokenName(string str)
        {
            if (!ycTokenToString.ContainsValue(str))
                return null;

            return ycTokenToString.First(pair => pair.Value == str).Key;
        }

        /// <summary>
        /// Returns string value from ycToken.
        /// For example if ycToken is LBRACE then method returns "("
        /// </summary>
        /// <param name="ycToken"></param>
        /// <returns></returns>
        public static string GetStringValue(string ycToken)
        {
            if (!ycTokenToString.ContainsKey(ycToken))
                return null;
            return ycTokenToString[ycToken];
        }

        public static void ReCreate()
        {
            ununique = new List<string>();
            ycTokenToString = new Dictionary<string, string>();
        }
    }
}
