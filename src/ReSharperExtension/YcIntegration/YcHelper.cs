using System.Collections.Generic;

namespace ReSharperExtension.YcIntegration
{
    public static class YcHelper
    {
        private static Dictionary<string, Dictionary<string, int>> allYcToInt =
            new Dictionary<string, Dictionary<string, int>>();

        public static void AddYcItem(string key, int number, string lang)
        {
            lang = lang.ToLowerInvariant();
            key = key.ToLowerInvariant();

            Dictionary<string, int> dict;
            
            if (allYcToInt.ContainsKey(lang))
            {
                dict = allYcToInt[lang];
            }
            else
            {
                var newDict = new Dictionary<string, int>();
                allYcToInt.Add(lang, newDict);
                dict = newDict;
            }
            
            if (!dict.ContainsKey(key))
            {
                dict.Add(key, number);
            }
        }

        public static int GetNumber (string lang, string key)
        {
            lang = lang.ToLowerInvariant();
            key = key.ToLowerInvariant();

            if (allYcToInt.ContainsKey(lang))
            {
                var dict = allYcToInt[lang];           
                return dict.ContainsKey(key) ? dict[key] : -1;
            }

            return -1;
        }
    }
}
