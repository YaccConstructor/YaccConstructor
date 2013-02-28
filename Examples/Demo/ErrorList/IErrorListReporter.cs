using System;
using System.Collections.ObjectModel;

namespace ErrorList
{
    public interface IErrorList
    {
        ObservableCollection<ErrorListDataEntry> DataBindingContext { get; set; }

        bool ErrorsVisible { get; set; }
        bool WarningsVisible { get; set; }
        bool MessagesVisible { get; set; }

        void ClearAll();
        void AddError(string description);
        void AddInformation(string description);
        void AddWarning(string description);
    }
}
