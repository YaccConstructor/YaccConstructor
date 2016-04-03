using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Xml.Serialization;
using System.IO;
using System.Linq;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.Util;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Settings
{
    /// <summary>
    /// Saves and loads configuration information about string-embedded languages. 
    /// </summary>
    internal static class ConfigurationManager
    {
        private static readonly ReSharperHelper<DocumentRange, ITreeNode> helper = ReSharperHelper<DocumentRange, ITreeNode>.Instance;

        private const string YcTempFolder = "YCPluginTempFolder";
        private const string hotspotFile = "Hotspots.xml";
        private const string xmlExtension = ".xml";

        private static readonly Dictionary<string, LanguageSettings> LoadToSettings = new Dictionary<string, LanguageSettings>();

        private static string GetYCFolderPath()
        {
            string localAppDataPath = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
            string ycFolderPath = Path.Combine(localAppDataPath, YcTempFolder);

            if (!Directory.Exists(ycFolderPath))
                Directory.CreateDirectory(ycFolderPath);

            return ycFolderPath;
        }

        internal static void SaveHotspotData(IEnumerable<HotspotModelView> hotspots)
        {
            string ycPath = GetYCFolderPath();

            string hotspotFullName = Path.Combine(ycPath, hotspotFile);
            if (File.Exists(hotspotFullName))
                File.Delete(hotspotFullName);

            FileStream fs = new FileStream(hotspotFullName, FileMode.CreateNew);
            XmlSerializer s = new XmlSerializer(typeof(ObservableCollection<HotspotModelView>));

            var collection = new ObservableCollection<HotspotModelView>(hotspots);
            s.Serialize(fs, collection);
        }

        internal static IEnumerable<HotspotModelView> LoadHotspotData()
        {
            string ycPath = GetYCFolderPath();
            string hotspotFullName = Path.Combine(ycPath, hotspotFile);

            if (!File.Exists(hotspotFullName))
                return new ObservableCollection<HotspotModelView>();

            var xmlSerializer = new XmlSerializer(typeof(ObservableCollection<HotspotModelView>));

            ObservableCollection<HotspotModelView> collection;
            using (var stringReader = new StreamReader(hotspotFullName))
            {
                collection = (ObservableCollection<HotspotModelView>)xmlSerializer.Deserialize(stringReader);
            }

            IEnumerable<string> availableLanguages = helper.GetAllLanguagesNames();

            Func<string, bool> langExists = one =>
            {
                return availableLanguages.Any(
                    langName => String.Equals(one, langName, StringComparison.InvariantCultureIgnoreCase));
            };

            IEnumerable<HotspotModelView> result = collection.Where(hotspotView => langExists(hotspotView.LanguageName));

            return result;
        }

        internal static LanguageSettings LoadLangSettings(string lang)
        {
            if (LoadToSettings.ContainsKey(lang))
                return LoadToSettings[lang];

            string ycPath = GetYCFolderPath();
            string fileFullPath = Path.Combine(ycPath, String.Format("{0}{1}", lang, xmlExtension));

            LanguageSettings settings;
            if (File.Exists(fileFullPath))
            {
                var xmlSerializer = new XmlSerializer(typeof(LanguageSettings));
                using (var stringReader = new StreamReader(fileFullPath))
                {
                    settings = (LanguageSettings)xmlSerializer.Deserialize(stringReader);
                }
            }
            else
            {
                settings = new LanguageSettings(lang);
                var availableTokens = helper.GetAvailableTokens(lang);

                foreach (string token in availableTokens)
                {
                    var tokenModel = new TokenInfoModelView
                    {
                        TokenName = token,
                        ColorId = ColorHelper.DefaultColor,
                    };
                    settings.TokensInfo.Add(tokenModel);
                }
            }

            LoadToSettings[lang] = settings;
            return settings;
        }

        internal static void SaveSettings(LanguageSettings settings)
        {
            string ycPath = GetYCFolderPath();
            string fileFullPath = Path.Combine(ycPath, String.Format("{0}{1}", settings.Language, xmlExtension));
            if (File.Exists(fileFullPath))
                File.Delete(fileFullPath);

            FileStream fs = new FileStream(fileFullPath, FileMode.CreateNew);
            XmlSerializer s = new XmlSerializer(typeof(LanguageSettings));

            s.Serialize(fs, settings);
        }
    }
}
