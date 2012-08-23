using System;
using System.Collections.Generic;
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
using System.IO;


namespace Yard.Development.Tools.UIComponents
{
	/// <summary>
	/// Interaction logic for YCOpenGrammar.xaml
	/// </summary>
	public partial class YCOpenGrammar : UserControl
	{
		public YCOpenGrammar()
		{
			this.InitializeComponent();            
		}
        
        public string GrammarFilePath
        {
            get { return tb_GrammarPath.Text; }
            private set { tb_GrammarPath.Text = value; }
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            System.Windows.Forms.OpenFileDialog ofd = new System.Windows.Forms.OpenFileDialog();
            ofd.Filter = "Grammar files (*.yrd)|*.yrd";
            ofd.Title = "Open grammar";

            if (ofd.ShowDialog() == System.Windows.Forms.DialogResult.OK && !string.IsNullOrEmpty(ofd.FileName))
                if (File.Exists(ofd.FileName))
                    GrammarFilePath = ofd.FileName;            
        }
	}
}