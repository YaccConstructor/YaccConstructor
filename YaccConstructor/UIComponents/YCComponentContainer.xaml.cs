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
    /// Interaction logic for YCComponentContainer.xaml
    /// </summary>
    public partial class YCComponentContainer : UserControl
    {
        public YCComponentContainer(string componentTypeName)
        {
            InitializeComponent();
            ComponentName = componentTypeName;
        }

        public YCComponentContainer()
        {
            InitializeComponent();            
        }

        public string ComponentName
        {
            get
            {
                return (string)this.lbl_ComponentName.Content;
            }
            set
            {
                this.lbl_ComponentName.Content = value + ":";
            }
        }

        public IEnumerable<string> ComponentsList
        {
            get
            {
                return (IEnumerable<string>)this.cb_Component.ItemsSource;
            }
            set
            {
                cb_Component.ItemsSource = value;
            }
        }

        public string SelectedConponent
        {
            get
            {
                return (string)cb_Component.SelectedValue;
            }
        }
    }
}
