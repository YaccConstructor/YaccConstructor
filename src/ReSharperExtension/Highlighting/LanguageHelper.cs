using System;
using System.Collections.Generic;
using System.Linq;
using ReSharperExtension.Settings;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Highlighting
{
    static class LanguageHelper
    {
        private static readonly List<LanguageWithColorInfo> availableLang = new List<LanguageWithColorInfo>();

        private static bool AreEqualString(string one, string two)
        {
            return String.Equals(one, two, StringComparison.InvariantCultureIgnoreCase);
        }

        public static string GetBrother(string lang, string str, Brother brother)
        {
            LanguageWithColorInfo languageWithColorInfo = availableLang.FirstOrDefault(item => AreEqualString(item.LanguageName, lang));
            if (languageWithColorInfo == null)
                return null;
            return languageWithColorInfo.GetBrother(str, brother);
        }

        public static void Update(string lang)
        {
            if (!availableLang.Exists(item => AreEqualString(item.LanguageName, lang)))
            {
                LanguageSettings settings = ConfigurationManager.LoadLangSettings(lang);
                Dictionary<string, TokenInfo> tokenInfo = settings.GetFullTokensInfo();

                var language = new LanguageWithColorInfo(lang.ToLowerInvariant(), tokenInfo);
                availableLang.Add(language);
            }
        }

        public static string GetColor(string lang, string token)
        {
            LanguageWithColorInfo languageWithColorInfo = availableLang.FirstOrDefault(item => AreEqualString(item.LanguageName, lang));
            if (languageWithColorInfo == null)
                return null;
            return languageWithColorInfo.GetColor(token.ToLowerInvariant());
        }

        public static int GetNumberFromYcName(string lang, string ycName)
        {
            LanguageWithColorInfo languageWithColorInfo = availableLang.FirstOrDefault(item => AreEqualString(item.LanguageName, lang));
            if (languageWithColorInfo == null)
                return -1;

            return languageWithColorInfo.GetNumber(ycName);
        }
    }

    internal class LanguageWithColorInfo
    {
        public string LanguageName { get; private set; }

        private readonly Dictionary<string, TokenInfo> tokenInfos;

        public LanguageWithColorInfo(string lang, Dictionary<string, TokenInfo> tokenInfos)
        {
            LanguageName = lang;
            this.tokenInfos = tokenInfos;
        }

        public string GetBrother(string ycName, Brother brother)
        {
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

        public string GetColor(string token)
        {
            if (tokenInfos.ContainsKey(token))
                return tokenInfos[token].Color;

            return ColorHelper.DefaultColor;
        }

        public int GetNumber(string ycName)
        {
            return YcHelper.GetNumber(LanguageName, ycName);
        }
    }

    internal class TokenInfo
    {
        public string LeftPair { get; set; }
        public string RightPair { get; set; }
        public string Color { get; set; }
    }

    internal enum Brother
    {
        Left,
        Right
    }
}
