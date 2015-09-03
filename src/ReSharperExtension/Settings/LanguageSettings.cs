using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Xml.Serialization;
using ReSharperExtension.Highlighting;

namespace ReSharperExtension.Settings
{
    [Serializable]
    public class LanguageSettings
    {
        [XmlElement("Language")]
        public string Language { get; set; }

        [XmlArray("Hotspots"), XmlArrayItem("Hotspot", typeof(HotspotModelView))]
        public ObservableCollection<HotspotModelView> Hotspots { get; set; }

        [XmlArray("Tokens"), XmlArrayItem("TokenInfo", typeof(TokenInfoModelView))]
        public ObservableCollection<TokenInfoModelView> TokensInfo { get; set; }

        [XmlArray("Matched"), XmlArrayItem("Pair", typeof(PairedTokens))]
        public ObservableCollection<PairedTokens> Pairs { get; set; }

        public LanguageSettings()
        {
        }

        public LanguageSettings(string language)
        {
            Language = language;
            Hotspots = new ObservableCollection<HotspotModelView>();
            TokensInfo = new ObservableCollection<TokenInfoModelView>();
            Pairs = new ObservableCollection<PairedTokens>();
        }

        internal Dictionary<string, TokenInfo> GetFullTokensInfo()
        {
            Dictionary<string, TokenInfo> res = new Dictionary<string, TokenInfo>();

            foreach (TokenInfoModelView tokenModel in TokensInfo)
            {
                var tokenInfo = new TokenInfo();
                string tokenName = tokenModel.TokenName.ToLowerInvariant();
                tokenInfo.Color = tokenModel.ColorId;

                PairedTokens t = Pairs.FirstOrDefault(pair => pair.LeftTokenName.ToLowerInvariant() == tokenName 
                                                    || pair.RightTokenName.ToLowerInvariant() == tokenName);
                if (t != null)
                {
                    if (t.LeftTokenName.ToLowerInvariant() == tokenName)
                        tokenInfo.RightPair = t.RightTokenName;
                    if (t.RightTokenName.ToLowerInvariant() == tokenName)
                        tokenInfo.LeftPair = t.LeftTokenName;
                }
                res[tokenName] = tokenInfo;
            }
            return res;
        }
    }
}
