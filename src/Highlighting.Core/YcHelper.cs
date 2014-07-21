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
            lang = lang.ToLower();
            key = key.ToLower();
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
                    if (strValue.numOfValues == Value.OneValue)
                    {
                        if (strValue.stringValue != value)
                        {
                            strValue.numOfValues = Value.ManyValues;
                            strValue.stringValue = null;
                        }
                    }
                }
                else
                {
                    dict.Add(key, new StringValue()
                    {
                        numOfValues = Value.OneValue,
                        stringValue = value,
                    });
                }

            }
        }

        public static string GetYcName(string lang, string str)
        {
            //string str = Yard.Generators.RNGLR.Helper._getLiteralName(s);
            if (string.IsNullOrEmpty(lang) || !allYcToString.ContainsKey(lang))
                return null;

            var dict = allYcToString[lang];

            if (string.IsNullOrEmpty(str))
                return null;

            return
                dict.FirstOrDefault(item => item.Value.numOfValues == Value.OneValue /*&& item.Key == str*/&& item.Value.stringValue == str)
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
                dict.FirstOrDefault(item => item.Value.numOfValues == Value.OneValue && item.Key == str)
                    .Value.stringValue;
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
        public Value numOfValues { get; set; }
        public string stringValue { get; set; }
    }
}
