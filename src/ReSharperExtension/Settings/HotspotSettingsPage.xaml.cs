using System.Collections.ObjectModel;
using System.Linq;
using System.Windows;
using JetBrains.UI.CrossFramework;
using JetBrains.UI.Options;
using JetBrains.Util;

namespace ReSharperExtension.Settings
{
    /// <summary>
    /// Interaction logic for HotspotSettingsPage.xaml
    /// </summary>
    [OptionsPage(Pid, "Hotspots", null, ParentId = YCSettingsPage.Pid)]
    public partial class HotspotSettingsPage : IOptionsPage
    {
        public const string Pid = "YaccConstructor.Hotspots.OptionsPage";

        private ObservableCollection<HotspotModelView> settings;

        public HotspotSettingsPage()
        {
			InitializeComponent();

            SetDefaultValues();
        }

        private void SetDefaultValues()
        {
            settings = ConfigurationManager.LoadHotspotData();
            gridHotspots.ItemsSource = settings;
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
                ConfigurationManager.SaveHotspotData(settings);
                Handler.UpdateHotspots(settings);
                return true;
            }
            return false;
        }

        public bool ValidatePage()
        {
            return settings.All(hotspotModel => hotspotModel.AmCorrect());
        }
        #endregion

        private void OnAddClicked(object sender, RoutedEventArgs e)
        {
            var newElem = new HotspotModelView();
            settings.Add(newElem);
        }

        private void OnRemoveClicked(object sender, RoutedEventArgs e)
        {
            HotspotModelView[] selected = gridHotspots.SelectedItems.SafeOfType<HotspotModelView>().ToArray();
            settings.RemoveRange(selected);
        }
    }
}
