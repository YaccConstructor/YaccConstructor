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
using System.Collections.ObjectModel;
using System.Windows.Controls.Primitives;

namespace ErrorList
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class ErrorListControl : UserControl, IErrorList
    {
        public ErrorListControl()
        {
            InitializeComponent();
            
            dataGrid1.DataContext = _dataContext; //todo
            SetTextBoxBindings();
        }

        #region IErrorListReporter

        public ObservableCollection<ErrorListDataEntry> DataBindingContext
        {
            get { return _dataContext.ErrorListData; }
            set
            {
                if (value == null)
                    throw new System.ArgumentNullException("Unable to bind to a null reference");

                _dataContext.ErrorListData = value;
            }
        }

        public bool ErrorsVisible
        {
            get
            {
                return tglBtnErrors.IsChecked.HasValue && tglBtnErrors.IsChecked.Value;
            }
            set
            {
                tglBtnErrors.IsChecked = value;
            }
        }

        public bool WarningsVisible
        {
            get
            {
                return tglBtnWarnings.IsChecked.HasValue && tglBtnWarnings.IsChecked.Value;
            }
            set
            {
                tglBtnWarnings.IsChecked = value;
            }
        }

        public bool MessagesVisible
        {
            get
            {
                return tglBtnMessages.IsChecked.HasValue && tglBtnMessages.IsChecked.Value;
            }
            set
            {
                tglBtnMessages.IsChecked = value;
            }
        }

        public void ClearAll()
        {
            _dataContext.ErrorListData.Clear();
        }

        public void AddError(string description)
        {
            _dataContext.AddError(description);
        }

        public void AddWarning(string description)
        {
            _dataContext.AddWarning(description);
        }

        public void AddInformation(string description)
        {
            _dataContext.AddInformation(description);
        }

        #endregion IErrorListReporter

        #region EventHandlers

        private void Errors_Checked(object sender, System.Windows.RoutedEventArgs e)
        {
            ToggleButton tgl = (ToggleButton)sender;
            _dataContext.ShowErrors = tgl.IsChecked.HasValue ? tgl.IsChecked.Value : false;
        }

        private void Errors_Unchecked(object sender, System.Windows.RoutedEventArgs e)
        {
            ToggleButton tgl = (ToggleButton)sender;
            _dataContext.ShowErrors = tgl.IsChecked.HasValue ? tgl.IsChecked.Value : false;
        }

        private void Warnings_Checked(object sender, System.Windows.RoutedEventArgs e)
        {
            ToggleButton tgl = (ToggleButton)sender;
            _dataContext.ShowWarnings = tgl.IsChecked.HasValue ? tgl.IsChecked.Value : false;
        }

        private void Warnings_Unchecked(object sender, System.Windows.RoutedEventArgs e)
        {
            ToggleButton tgl = (ToggleButton)sender;
            _dataContext.ShowWarnings = tgl.IsChecked.HasValue ? tgl.IsChecked.Value : false;
        }

        private void Informations_Checked(object sender, System.Windows.RoutedEventArgs e)
        {
            ToggleButton tgl = (ToggleButton)sender;
            _dataContext.ShowInformations = tgl.IsChecked.HasValue ? tgl.IsChecked.Value : false;
        }

        private void Informations_Unchecked(object sender, System.Windows.RoutedEventArgs e)
        {
            ToggleButton tgl = (ToggleButton)sender;
            _dataContext.ShowInformations = tgl.IsChecked.HasValue ? tgl.IsChecked.Value : false;
        }

        #endregion EventHandlers

        private void SetTextBoxBindings()
        {
            txtErrors.DataContext = _dataContext;
            txtWarnings.DataContext = _dataContext;
            txtMessages.DataContext = _dataContext;
        }

        private ErrorListDataModel _dataContext = new ErrorListDataModel();
    }
}
