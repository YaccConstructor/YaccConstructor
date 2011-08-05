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
        public MainWindow()
        {            
            InitializeComponent();
            yc_FrontendsContainer.ComponentName = "Frontend";
            yc_FrontendsContainer.ComponentsList = Yard.Core.FrontendsManager.AvailableFrontends;
            yc_GeneratorsContainer.ComponentName = "Generator";
            yc_GeneratorsContainer.ComponentsList = Yard.Core.GeneratorsManager.AvailableGenerators;            
        }

        private void btn_Run_Click(object sender, RoutedEventArgs e)
        {
            tb_GrammarText.Text = (new System.IO.StreamReader(yc_OpenGrammar.GrammarFilePath)).ReadToEnd();
            tbc_Graphs.Items.Clear();
            var fe = Yard.Core.FrontendsManager.Frontend(yc_FrontendsContainer.SelectedConponent);
            var il = fe.ParseGrammar(yc_OpenGrammar.GrammarFilePath);
            var be = Yard.Core.GeneratorsManager.Generator(yc_GeneratorsContainer.SelectedConponent);
            var res = Development.Tools.TablesPrinter.formatRaccGenresult((GNESCCGenerator)be,il);            
            foreach (var g in res)
                tbc_Graphs.Items.Add(new UIComponents.YCGraphEditor() { ClipToBounds = true, Graph = g });
            tb_TableView.Text = Development.Tools.TablesPrinter.res;
        }
    }
}
