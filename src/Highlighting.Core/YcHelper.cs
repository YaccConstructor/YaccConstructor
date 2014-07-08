using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Util.Concurrency;

namespace Highlighting.Core
{
    public static class YcHelper
    {
        /// <summary>
        /// For each language name returns ycTokenToString dictiionary
        /// </summary>
        private static Dictionary<string, Dictionary<string, string>> allYcToString = new Dictionary<string, Dictionary<string, string>>();
        //private static readonly Dictionary<string, List<string>> allUnunique = new Dictionary<string, List<string>>();
        private static readonly LockObject lockObject = new LockObject();

        public static void AddYcItem(string key, string value, string lang)
        {
            lang = lang.ToLower();
            if (String.IsNullOrEmpty(key) ||
                String.IsNullOrEmpty(value))
                return;

            lock (lockObject)
            {
                if (!allYcToString.ContainsKey(lang))
                {
                    allYcToString.Add(lang, new Dictionary<string, string>());
                    //allUnunique.Add(lang, new List<string>());
                }
                var ycTokenToString = allYcToString[lang];
                //var ununique = allUnunique[lang];

                if (ycTokenToString.ContainsKey(key))
                {
                    if (ycTokenToString[key] != value)
                    {
                        ycTokenToString.Remove(key);
                        //ununique.Add(key);
                    }
                }
                else
                {
                    ycTokenToString.Add(key, value);
                }
            }
        }

        /// <summary>
        /// Returns ycToken from string value.
        /// For example if str is "(" then method returns LBRACE
        /// </summary>
        /// <param name="str"></param>
        /// <param name="lang">Name of language (Calc, JSON, TSQL)</param>
        /// <returns></returns>
        public static string GetYcTokenName(string str, string lang)
        {
            if (!allYcToString.ContainsKey(lang)) return null;

            var ycTokenToString = allYcToString[lang];
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
        public static string GetStringValue(string ycToken, string lang)
        {
            if (!allYcToString.ContainsKey(lang)) return null;

            var ycTokenToString = allYcToString[lang];
            if (!ycTokenToString.ContainsKey(ycToken))
                return null;

            return ycTokenToString[ycToken];
        }


        /// <summary>
        /// Removes unnecessary key-value from table
        /// </summary>
        public static void Update(string lang, Dictionary<string, string> dictionary)
        {
            if (dictionary.Count == 0)
                return;

            Dictionary<string, string> ycToString = allYcToString[lang];

            foreach (KeyValuePair<string, string> item in ycToString)
            {
                if (dictionary.ContainsKey(item.Key) || dictionary.ContainsValue(item.Key))
                    continue;
                dictionary.Remove(item.Key);
            }
        }
    }
}
