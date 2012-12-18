using System.Windows;
using System.Windows.Controls;

namespace ErrorList
{
    /// <summary>
    /// Interaction logic for TestPage.xaml
    /// </summary>
    public partial class TestPage : Page
    {
        public TestPage()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            IErrorList errLstCtrl = errorList1 as IErrorList;

            errLstCtrl.AddError("Error unable to do something \"Name: Write PHP\"");
            errLstCtrl.AddError("Error unable to do something \"Name: Write Flash\"");
            errLstCtrl.AddWarning("Error unable to do something \"Name: Program in F#, yet\"");
            errLstCtrl.AddInformation("Note: I need a better hobby than wasting my lunch coding..");
        }
    }
}
