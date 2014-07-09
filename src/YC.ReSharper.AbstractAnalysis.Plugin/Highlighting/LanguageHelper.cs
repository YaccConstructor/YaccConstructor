using System;
using System.Collections.Generic;
using System.Linq;
using Highlighting.Core;
using JetBrains.ReSharper.Feature.Services.Intentions;
using JetBrains.ReSharper.Feature.Services.Tips;
using JetBrains.ReSharper.Psi.Impl;
using Yard.Core;

namespace YC.ReSharper.AbstractAnalysis.Plugin.Highlighting
{
    static class LanguageHelper
    {
        private static List<Language> availableLang = new List<Language>();

        public static string GetBrother(string lang, string str, Brother brother)
        {
            Language language = availableLang.FirstOrDefault(item => item.LanguageName == lang.ToLower());
            if (language == null)
                return null;
            return language.GetBrother(str, brother);
        }

        public static void Update(string lang, Dictionary<string, TokenInfo> tokenInfo)
        {
            if (!availableLang.Exists(item => item.LanguageName == lang.ToLower()))
            {
                var language = new Language(lang.ToLower(), tokenInfo);
                availableLang.Add(language);
            }
        }

        //public static void AddMatchedTokens(Dictionary<string, string> matched, string lang)
        //{
        //    Language language = availableLang.FirstOrDefault(item => item.LanguageName == lang);
        //    if (language == null)
        //        throw new Exception("Error in LanguageHelper");
        //    language.AddMatchedPaired(matched);
        //}

        public static string GetColor(string lang, string token)
        {
            Language language = availableLang.FirstOrDefault(item => item.LanguageName == lang.ToLower());
            if (language == null)
                return null;
            return language.GetColor(token.ToLower());
        }
    }

    class Language
    {
        private enum MatchedParsed
        {
            TRUE, 
            FALSE
        }
        
        public string LanguageName { get; private set; }

        private Dictionary<string, TokenInfo> tokenInfos = new Dictionary<string, TokenInfo>();
        
        private MatchedParsed state = MatchedParsed.FALSE;

        public Language(string lang, Dictionary<string, TokenInfo> tokenInfos)
        {
            LanguageName = lang;
            this.tokenInfos = tokenInfos;
            Update();
        }

        public string GetBrother(string str, Brother brother)
        {
            string ycName = GetYcName(str);

            if (string.IsNullOrEmpty(ycName))
                return null;

            string pair = null;
            if (tokenInfos.ContainsKey(ycName))
            {
                string ycPair = null;
                if (brother == Brother.Left)
                {
                    ycPair =  tokenInfos[ycName].LeftPair;

                }
                else if (brother == Brother.Right)
                {
                    ycPair = tokenInfos[ycName].RightPair;
                }
                pair = GetStringFromYcName(ycPair);
            }
            
            return pair;
        }

        /// <summary>
        /// Maps token name from YaccConstructor. For example if str is "(" then returned value is "LBRACE"
        /// This method contains definition only for paired tokens.
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        private string GetYcName(string str)
        {
            return YcHelper.GetYcName(LanguageName, str);
        }

        private string GetStringFromYcName(string str)
        {
            return YcHelper.GetStringName(LanguageName, str);
        }

        //private string GetYcValue(string ycName)
        //{
        //    if (string.IsNullOrEmpty(ycName))
        //        return null;

        //    if (ycToString.ContainsKey(ycName))
        //        return ycToString[ycName];

        //    return null;
        //}

        //public void AddMatchedPaired(Dictionary<string, string> pairs)
        //{
        //    state = MatchedParsed.TRUE;
        //}

        public string GetColor(string token)
        {
            if (tokenInfos.ContainsKey(token))
                return tokenInfos[token].Color;

            return ColorHelper.DefaultColor;
        }

        private void Update()
        {
            //Dictionary<string, string> dict = YcHelper.GetYcTokenToString(LanguageName);

            //YcHelper.Clear(LanguageName);
        }
    }

    class TokenInfo
    {
        //possible it is unnecessary
        public string YcName { get; set; }
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
