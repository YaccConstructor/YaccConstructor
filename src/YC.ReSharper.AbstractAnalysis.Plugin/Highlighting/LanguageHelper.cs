using System;
using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    static class LanguageHelper
    {
        private static List<Language> availableLang = new List<Language>();

        public static string GetBrother(string lang, string str, Brother brother)
        {
            Language language = availableLang.FirstOrDefault(item => item.LanguageName == lang.ToLowerInvariant());
            if (language == null)
                return null;
            return language.GetBrother(str, brother);
        }

        public static void Update(string lang, Dictionary<string, TokenInfo> tokenInfo)
        {
            if (!availableLang.Exists(item => item.LanguageName == lang.ToLowerInvariant()))
            {
                var language = new Language(lang.ToLowerInvariant(), tokenInfo);
                availableLang.Add(language);
            }
        }

        public static string GetColor(string lang, string token)
        {
            Language language = availableLang.FirstOrDefault(item => item.LanguageName == lang.ToLowerInvariant());
            if (language == null)
                return null;
            return language.GetColor(token.ToLowerInvariant());
        }
    }

    class Language
    {
        public string LanguageName { get; private set; }

        private Dictionary<string, TokenInfo> tokenInfos = new Dictionary<string, TokenInfo>();

        public Language(string lang, Dictionary<string, TokenInfo> tokenInfos)
        {
            LanguageName = lang;
            this.tokenInfos = tokenInfos;
        }

        public string GetBrother(string str, Brother brother)
        {
            string ycName = GetYcName(str);

            if (String.IsNullOrEmpty(ycName) || 
                !tokenInfos.ContainsKey(ycName))
                return null;

            switch (brother)
            {
                case Brother.Left:
                    return tokenInfos[ycName].LeftPair;
                case Brother.Right:
                    return tokenInfos[ycName].RightPair;
                default:
                    return null;
            }
        }

        /// <summary>
        /// Maps token name from YaccConstructor. For example if str is "(" then returned value is "LBRACE"
        /// This method contains definition only for paired tokens.
        /// </summary>
        private string GetYcName(string str)
        {
            return YcHelper.GetYcName(LanguageName, str);
        }

        public string GetColor(string token)
        {
            if (tokenInfos.ContainsKey(token))
                return tokenInfos[token].Color;

            return ColorHelper.DefaultColor;
        }
    }

    class TokenInfo
    {
        public string LeftPair { get; set; }
        public string RightPair { get; set; }
        public string Color { get; set; }
    }

    public enum Brother
    {
        Left,
        Right
    }
}
