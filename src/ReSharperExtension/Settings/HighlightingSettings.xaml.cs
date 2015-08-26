using JetBrains.UI.CrossFramework;
using JetBrains.UI.Options;

namespace ReSharperExtension.Settings
{
    /// <summary>
    /// Interaction logic for HighlightingSettings.xaml
    /// </summary>
    [OptionsPage(Pid, "Highlihgting", null, ParentId = YCSettingsPage.Pid)]
    public partial class HighlightingSettings : IOptionsPage
    {
        public const string Pid = "YaccConstructor.HighlightingSettings.OptionsPage";
        public HighlightingSettings()
        {
            InitializeComponent();
        }

        #region IOptionsPage members
        public bool OnOk()
        {
            return true;
        }

        public bool ValidatePage()
        {
            return true;
        }

        public EitherControl Control { get; private set; }

        public string Id
        {
            get { return Pid; }
        }
        #endregion
    }

}
