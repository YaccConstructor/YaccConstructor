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
        private static readonly LockObject lockObject = new LockObject();

        public static void AddYcItem(string key, string value, string lang)
        {
            lang = lang.ToLowerInvariant();
            key = key.ToLowerInvariant();
            if (String.IsNullOrEmpty(key) || String.IsNullOrEmpty(value))
                return;

            lock (lockObject)
            {
                if (!allYcToString.ContainsKey(lang))
                {
                    allYcToString.Add(lang, new Dictionary<string, StringValue>());
                }

                var dict = allYcToString[lang];

                if (dict.ContainsKey(key))
                {
                    var strValue = dict[key];
                    if (strValue.NumOfValues == Value.OneValue)
                    {
                        if (strValue.TextValue != value)
                        {
                            strValue.NumOfValues = Value.ManyValues;
                            strValue.TextValue = null;
                        }
                    }
                }
                else
                {
                    dict.Add(key, new StringValue()
                    {
                        NumOfValues = Value.OneValue,
                        TextValue = value,
                    });
                }

            }
        }

        public static string GetYcName(string lang, string str)
        {
            if (string.IsNullOrEmpty(lang) || !allYcToString.ContainsKey(lang))
                return null;

            var dict = allYcToString[lang];

            if (string.IsNullOrEmpty(str))
                return null;

            return
                dict.FirstOrDefault(item => item.Value.NumOfValues == Value.OneValue && item.Value.TextValue == str)
                    .Key;

        }

        public static string GetStringName(string lang, string str)
        {
            if (string.IsNullOrEmpty(lang) || !allYcToString.ContainsKey(lang))
                return null;
            
            var dict = allYcToString[lang];

            if (string.IsNullOrEmpty(str))
                return null;

            return
                dict.FirstOrDefault(item => 
                    item.Value.NumOfValues == Value.OneValue && item.Key == str)
                    .Value.TextValue;
        }
    }

    public enum Value
    {
        NoValue,
        OneValue,
        ManyValues
    }

    public class StringValue
    {
        public Value NumOfValues { get; set; }
        public string TextValue { get; set; }
    }
}
