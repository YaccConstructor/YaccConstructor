using JetBrains.UI.CrossFramework;
using JetBrains.UI.Options;
using JetBrains.UI.Options.OptionPages;

namespace ReSharperExtension.Settings
{
    /// <summary>
    /// Interaction logic for YCSettingsPage.xaml
    /// </summary>
    [OptionsPage(Pid, "YaccConstructor", null, ParentId = EnvironmentPage.Pid)]
    public partial class YCSettingsPage : IOptionsPage
    {
        public const string Pid = "YaccConstructor.OptionsPage";

        public YCSettingsPage()
        {
            InitializeComponent();
        }

        #region IOptionsPage
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
            get
            {
                return Pid;
            }
        }
        #endregion
    }
}
