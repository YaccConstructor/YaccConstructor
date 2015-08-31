using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Xml.Serialization;
using System.IO;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;
using ReSharperExtension.Highlighting;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Settings
{
    /// <summary>
    /// Saves and loads configuration information about string-embedded languages. 
    /// </summary>
    public static class ConfigurationManager
    {
        private static ReSharperHelper<DocumentRange, ITreeNode> helper = ReSharperHelper<DocumentRange, ITreeNode>.Instance;

        private const string YcTempFolder = "YCPluginTempFolder";
        private const string hotspotFile = "Hotspots.xml";
        private const string xmlExtension = ".xml";

        private static Dictionary<string, LanguageSettings> LoadToSettings = new Dictionary<string, LanguageSettings>();

        private static string GetYCFolderPath()
        {
            string localAppDataPath = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);
            string ycFolderPath = Path.Combine(localAppDataPath, YcTempFolder);

            if (!Directory.Exists(ycFolderPath))
                Directory.CreateDirectory(ycFolderPath);

            return ycFolderPath;
        }

        public static void SaveHotspotData(IEnumerable<HotspotModelView> hotspots)
        {
            string ycPath = GetYCFolderPath();

            string hotspotFullName = Path.Combine(ycPath, hotspotFile);
            if (File.Exists(hotspotFullName))
                File.Delete(hotspotFullName);

            FileStream fs = new FileStream(hotspotFullName, FileMode.CreateNew);
            XmlSerializer s = new XmlSerializer(typeof(ObservableCollection<HotspotModelView>));

            var collection = new List<HotspotModelView>(hotspots);
            s.Serialize(fs, collection);
        }

        public static ObservableCollection<HotspotModelView> LoadHotspotData()
        {
            string ycPath = GetYCFolderPath();

            string hotspotFullName = Path.Combine(ycPath, hotspotFile);

            if (!File.Exists(hotspotFullName))
            {
                return new ObservableCollection<HotspotModelView>(defaultHotspot);
            }

            var xmlSerializer = new XmlSerializer(typeof(ObservableCollection<>));
            
            var stringReader = new StreamReader(hotspotFullName);
            ObservableCollection<HotspotModelView> collection = (ObservableCollection<HotspotModelView>)xmlSerializer.Deserialize(stringReader);
            stringReader.Close();
            
            return collection;
        }

        private static List<HotspotModelView> defaultHotspot = 
            new List<HotspotModelView>
            {
                new HotspotModelView
            {
                LanguageName = "Calc",
                ClassName = "Program",
                MethodName = "Eval",
                ArgumentPosition = 0,
                ReturnedType = "int",
            },
            new HotspotModelView
            {
                LanguageName = "TSQL",
                MethodName = "ExecuteImmediate",
                ClassName = "Program",
                ArgumentPosition = 0,
                ReturnedType = "void",
            },
            new HotspotModelView
            {
                LanguageName = "ExtCalc",
                ClassName = "Program",
                MethodName = "ExtEval",
                ArgumentPosition = 0,
                ReturnedType = "int",
            },
            }
            ;
        private static HotspotModelView GetDefaultHotspot(string lang)
        {
            return defaultHotspot.Find(hotspot => hotspot.LanguageName.ToLowerInvariant() == lang.ToLowerInvariant());
        }

        public static LanguageSettings LoadLangSettings(string lang)
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
                var hotspot = GetDefaultHotspot(lang);
                if (hotspot != null)
                    settings.Hotspots.Add(hotspot);
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
