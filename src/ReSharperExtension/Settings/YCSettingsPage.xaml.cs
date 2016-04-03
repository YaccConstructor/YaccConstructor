using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Xml.Serialization;
using JetBrains.DocumentModel;
using JetBrains.ReSharper.Psi.Tree;
using JetBrains.UI.CrossFramework;
using JetBrains.UI.Options;
using JetBrains.UI.Options.OptionPages;
using JetBrains.Util;
using ReSharperExtension.YcIntegration;

namespace ReSharperExtension.Settings
{
    /// <summary>
    /// Interaction logic for YCSettingsPage.xaml
    /// </summary>
    [OptionsPage(Pid, "YaccContructor settings", null, ParentId = EnvironmentPage.Pid)]
    public partial class YCSettingsPage : IOptionsPage
    {
        private const string Pid = "YCSettings";

        private static Dictionary<string, LanguageSettings> Cache;
        private readonly ReSharperHelper<DocumentRange, ITreeNode> helper = ReSharperHelper<DocumentRange, ITreeNode>.Instance;
        private string currentLang;

        public YCSettingsPage()
        {
			InitializeComponent();

            if (Cache == null)
            {
                InitCache();
            }

            langView.ItemsSource = GetAvailableLangs();
            langView.SelectedItem = currentLang;
            SetDefaultValuesForLang();
        }

        private void InitCache()
        {
            Cache = new Dictionary<string, LanguageSettings>();
            IEnumerable<string> availableLangs = GetAvailableLangs();

            foreach (string lang in availableLangs)
            {
                LanguageSettings langSettings = ConfigurationManager.LoadLangSettings(lang);
                Cache.Add(lang, langSettings);
            }
        }

        private void SetDefaultValuesForLang()
        {
            if (String.IsNullOrEmpty(currentLang))
                return;

            LanguageSettings setting = Cache[currentLang];
            gridHotspots.ItemsSource = setting.Hotspots;

            tokenToColor.ItemsSource = setting.TokensInfo;
            colorColumn.ItemsSource = ColorHelper.GetColors();

            LeftElements.ItemsSource = setting.TokensInfo;
            RightElements.ItemsSource = setting.TokensInfo;

            LeftRightSymbols.ItemsSource = Cache[currentLang].Pairs;
        }

        private ObservableCollection<string> GetAvailableLangs()
        {
            IEnumerable<string> allLangs = helper.GetAllLanguagesNames();
            if (allLangs.Any())
            {
                currentLang = allLangs.First();
            }
            return new ObservableCollection<string>(allLangs);
        }

        private void SaveData()
        {
            foreach (LanguageSettings settings in Cache.Values)
            {
                ConfigurationManager.SaveSettings(settings);
            }
        }

        private void SaveHotspots()
        {
            var hotspots = new List<HotspotModelView>();
            foreach (LanguageSettings settings in Cache.Values)
            {
                hotspots.AddRange(settings.Hotspots);
            }

            if (hotspots.Any())
            {
                ConfigurationManager.SaveHotspotData(hotspots);
            }
            Handler.UpdateHotspots(hotspots);
        }

        #region IOptionPage members
        public EitherControl Control
        {
            get { return this; }
        }

        public string Id
        {
            get { return Pid; }
        }

        public bool OnOk()
        {
            if (ValidatePage())
            {
                SaveData();
                SaveHotspots();
                return true;
            }
            return false;
        }

        public bool ValidatePage()
        {
            if (String.IsNullOrEmpty(currentLang))
                return true;

            return Cache[currentLang].Hotspots.All(hotspotModel => hotspotModel.AmCorrect());
        }
        #endregion

        private void OnAddHotspotClicked(object sender, RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(currentLang))
                return;
            
            var newElem = new HotspotModelView {LanguageName = currentLang};
            Cache[currentLang].Hotspots.Add(newElem);
        }

        private void OnRemoveHotspotClicked(object sender, RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(currentLang))
                return;

            HotspotModelView[] selected = gridHotspots.SelectedItems.SafeOfType<HotspotModelView>().ToArray();
            Cache[currentLang].Hotspots.RemoveRange(selected);
        }

        private void langViewSelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            currentLang = langView.SelectedItem as string;
            SetDefaultValuesForLang();
        }

        private void OnAddPairClicked(object sender, RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(currentLang))
                return;

            LanguageSettings settings = Cache[currentLang];
            settings.Pairs.Add(new PairedTokens());
        }

        private void OnRemovePairClicked(object sender, RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(currentLang))
                return;
            
            LanguageSettings settings = Cache[currentLang];
            settings.Pairs.Remove(LeftRightSymbols.SelectedItem as PairedTokens);
        }

        private void OnEnterClick(object sender, AddingNewItemEventArgs e)
        {
            (e.NewItem as HotspotModelView).LanguageName = currentLang;
        }
    }

    [Serializable]
    public class PairedTokens
    {
        [XmlElement("Left")]
        public string LeftTokenName { get; set; }
        [XmlElement("Right")]
        public string RightTokenName { get; set; }

        public PairedTokens()
        {
        }

        public PairedTokens(string left, string right)
        {
            LeftTokenName = left;
            RightTokenName = right;
        }
    }
}
