using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Util.Concurrency;

namespace Highlighting.Core
{
    public static class YcHelper
    {
        /// <summary>
        /// For each language name returns ycTokenToString dictionary
        /// </summary>
        private static Dictionary<string, Dictionary<string, StringValue>> allYcToString = new Dictionary<string, Dictionary<string, StringValue>>();

        public static void AddYcItem(string key, int ycNumber, string lang)
        {
            lang = lang.ToLowerInvariant();
            key = key.ToLowerInvariant();
            if (String.IsNullOrEmpty(key))
                return;

            if (!allYcToString.ContainsKey(lang))
            {
                allYcToString.Add(lang, new Dictionary<string, StringValue>());
            }

            var dict = allYcToString[lang];

            if (dict.ContainsKey(key))
            {
                StringValue strValue = dict[key];
                if (strValue.NumOfValues == Value.OneValue)
                {
                    strValue.NumOfValues = Value.ManyValues;
                }
            }
            else
            {
                dict.Add(key, new StringValue()
                {
                    NumOfValues = Value.OneValue,
                    YcNumber = ycNumber,
                });
            }
        }

        public static int GetNumber(string lang, string key)
        {
            if (string.IsNullOrEmpty(lang) || !allYcToString.ContainsKey(lang))
                return -1;

            var dict = allYcToString[lang];

            if (string.IsNullOrEmpty(key) || !dict.ContainsKey(key))
                return -1;

            return dict[key].YcNumber;
        }
    }

    public enum Value
    {
        OneValue,
        ManyValues
    }

    public class StringValue
    {
        public Value NumOfValues { get; set; }
        public int YcNumber { get; set; }
    }
}
