using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Xml.Serialization;
using System.IO;
using System.Xml;

namespace ReSharperExtension.Settings
{
    public static class ConfigurationManager
    {
        private const string YcTempFolder = "YCPluginTempFolder";
        private const string hotspotConfigurationName = "Hotspots.xml";

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

            string hotspotFullName = Path.Combine(ycPath, hotspotConfigurationName);
            if (File.Exists(hotspotFullName))
                File.Delete(hotspotFullName);

            FileStream fs = new FileStream(hotspotFullName, FileMode.CreateNew);
            XmlSerializer s = new XmlSerializer(typeof(HotspotWrapperCollection));

            var collection = new HotspotWrapperCollection
            {
                Collection = new List<HotspotModelView>(hotspots)
            };

            s.Serialize(fs, collection);
        }

        public static ObservableCollection<HotspotModelView> LoadHotspotData()
        {
            string ycPath = GetYCFolderPath();

            string hotspotFullName = Path.Combine(ycPath, hotspotConfigurationName);

            if (!File.Exists(hotspotFullName))
            {
                return GetDefaultSetting();
            }

            var xmlSerializer = new XmlSerializer(typeof(HotspotWrapperCollection));
            
            var stringReader = new StreamReader(hotspotFullName);
            HotspotWrapperCollection collection = (HotspotWrapperCollection)xmlSerializer.Deserialize(stringReader);
            stringReader.Close();
            
            return new ObservableCollection<HotspotModelView>(collection.Collection);
        }

        private static ObservableCollection<HotspotModelView> GetDefaultSetting()
        {
            var calcHotspot = new HotspotModelView
            {
                LanguageName = "Calc",
                ClassName = "Program",
                MethodName = "Eval",
                ArgumentPosition = 0,
                ReturnedType = "int",
            };

            var tsqlCalcHotspot = new HotspotModelView
            {
                LanguageName = "TSQL",
                MethodName = "ExecuteImmediate",
                ClassName = "Program",
                ArgumentPosition = 0,
                ReturnedType = "void",
            };

            var extCalcHotspot = new HotspotModelView
            {
                LanguageName = "ExtCalc",
                ClassName = "Program",
                MethodName = "ExtEval",
                ArgumentPosition = 0,
                ReturnedType = "int",
            };

            return new ObservableCollection<HotspotModelView>
                    {
                        calcHotspot, 
                        tsqlCalcHotspot, 
                        extCalcHotspot
                    };
        }
    }
}
