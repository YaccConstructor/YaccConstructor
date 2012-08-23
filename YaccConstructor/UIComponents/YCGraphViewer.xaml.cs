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

namespace Yard.Development.Tools.UIComponents
{
    /// <summary>
    /// Interaction logic for YCGraphEditor.xaml
    /// </summary>
    public partial class YCGraphEditor : UserControl
    {
        private Microsoft.Glee.GraphViewerGdi.GViewer viewer = new Microsoft.Glee.GraphViewerGdi.GViewer();
        public YCGraphEditor()
        {
            InitializeComponent();                        
            this.windowsFormsHost1.Child = viewer;            
        }

        public Microsoft.Glee.Drawing.Graph Graph
        {
            set
            {
                viewer.Graph = value;
            }
        }
    }
}
