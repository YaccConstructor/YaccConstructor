using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Yard.Generators.GNESCCGenerator;

namespace Yard.Development.Tools.FrontendImpl
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        Yard.Core.GeneratorsManager.GeneratorsManager GeneratorsManager = new Yard.Core.GeneratorsManager.GeneratorsManager();
        Yard.Core.FrontendsManager.FrontendsManager FrontendsManager = new Yard.Core.FrontendsManager.FrontendsManager();
        public MainWindow()
        {
            InitializeComponent();
            yc_FrontendsContainer.ComponentName = "Frontend";
            yc_FrontendsContainer.ComponentsList = FrontendsManager.Available;
            yc_GeneratorsContainer.ComponentName = "Generator";
            yc_GeneratorsContainer.ComponentsList = GeneratorsManager.Available;
        }

        private void btn_Run_Click(object sender, RoutedEventArgs e)
        {
            
            try
            {
                tb_GrammarText.Text = (new System.IO.StreamReader(yc_OpenGrammar.GrammarFilePath)).ReadToEnd();
                tbc_Graphs.Items.Clear();
                var fe = FrontendsManager.Component (yc_FrontendsContainer.SelectedConponent);
                var il = fe.Value.ParseGrammar(yc_OpenGrammar.GrammarFilePath);
            
                var be = GeneratorsManager.Component(yc_GeneratorsContainer.SelectedConponent);
                var res = Development.Tools.TablesPrinter.formatRaccGenresult((GNESCCGenerator)be.Value, il);
                foreach (var g in res)
                    tbc_Graphs.Items.Add(new UIComponents.YCGraphEditor() { ClipToBounds = true, Graph = g });
                tb_TableView.Text = Development.Tools.TablesPrinter.res;
            }
            catch
            {

                Console.WriteLine("This generator or frontend not found.");
            }
        }
    }
}
