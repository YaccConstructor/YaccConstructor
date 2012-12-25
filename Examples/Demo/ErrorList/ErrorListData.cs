using System.Collections.ObjectModel;
using System.Linq;
using System;
using System.Windows.Documents;
using System.Collections.Generic;
using System.ComponentModel;

namespace ErrorList
{    
    public enum ErrorListLevel
    {
        Error,
        Warning,
        Information
    }

    public class ErrorListDataModel : INotifyPropertyChanged
    {                
        public string ErrorsText
        {
            get 
            {
                return string.Format("{0} Errors", _errorListData.Count(ed => ed.Level == ErrorListLevel.Error));
            }
            set { }
        }

        public string WarningsText
        {
            get 
            {
                return string.Format("{0} Warnings", _errorListData.Count(ed => ed.Level == ErrorListLevel.Warning));
            }
            set { }
        }

        public string InformationsText
        {
            get 
            {
                return string.Format("{0} Informations", _errorListData.Count(ed => ed.Level == ErrorListLevel.Information));
            }
            set { }
        }

        public void AddError(string description)
        {
            _errorListData.Add(new ErrorListDataEntry { Description = description, Level = ErrorListLevel.Error });
            SetView();
        }

        public void AddWarning(string description)
        {
            _errorListData.Add(new ErrorListDataEntry { Description = description, Level = ErrorListLevel.Warning });
            SetView();
        }

        public void AddInformation(string description)
        {
            _errorListData.Add(new ErrorListDataEntry { Description = description, Level = ErrorListLevel.Information });
            SetView();
        }

        public ObservableCollection<ErrorListDataEntry> ErrorListData 
        {
            get
            {
                return _errorListDataView;
            }
            internal set
            {
                _errorListData = value;
                SetView();
            }
        }


        public bool ShowErrors
        {
            set
            {
                _showErrors = value;
                SetView();
            }
        }

        public bool ShowWarnings
        {
            set
            {
                _showWarnings = value;
                SetView();
            }
        }

        public bool ShowInformations
        {
            set
            {
                _showInformations = value;
                SetView();
            }
        }

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;

        #endregion

        private void SetView()
        {
            var selectedLevels = new List<ErrorListLevel>();
            if (_showErrors)
                selectedLevels.Add(ErrorListLevel.Error);
            if (_showWarnings)
                selectedLevels.Add(ErrorListLevel.Warning);
            if (_showInformations)
                selectedLevels.Add(ErrorListLevel.Information);

            _errorListDataView.Clear();            
            var selectedErrors = _errorListData.Where(ed => selectedLevels.Contains(ed.Level));
            foreach (var selectedError in selectedErrors)
                _errorListDataView.Add(selectedError);

            if (PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs("ErrorsText"));
                PropertyChanged(this, new PropertyChangedEventArgs("WarningsText"));
                PropertyChanged(this, new PropertyChangedEventArgs("InformationsText"));
            }
            
        }

        private ObservableCollection<ErrorListDataEntry> _errorListData = new ObservableCollection<ErrorListDataEntry>();
        private ObservableCollection<ErrorListDataEntry> _errorListDataView = new ObservableCollection<ErrorListDataEntry>();

        private bool _showErrors = true;
        private bool _showWarnings = false;
        private bool _showInformations = false;

       
    }

    public class ErrorListDataEntry
    {
        public string Description { get; set; }
        public ErrorListLevel Level 
        {
            get { return _level; }
            set
            {
                _level = value;
                switch (_level)
                {
                    case ErrorListLevel.Error:
                        this.ErrorIconSrc = ErrorIcon_Rel_Path;
                        break;
                    case ErrorListLevel.Warning:
                        this.ErrorIconSrc = WarningIcon_Rel_Path;
                        break;
                    case ErrorListLevel.Information:
                        this.ErrorIconSrc = InformationIcon_Rel_Path;
                        break;
                }                
            }
        }

        public string ErrorIconSrc { get; private set; }

        private ErrorListLevel _level = ErrorListLevel.Error;

        private const string ErrorIcon_Rel_Path = "./Images/delete.png";
        private const string WarningIcon_Rel_Path = "./Images/warning.png";
        private const string InformationIcon_Rel_Path = "./Images/information.png";
    }
}
