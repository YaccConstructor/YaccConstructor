using System.ComponentModel;
using System.Xml.Serialization;
using JetBrains.Annotations;

namespace ReSharperExtension.Settings
{
    public class ColorModelView : INotifyPropertyChanged
    {
        private string viewName;

        [XmlElement("ViewName")]
        public string ViewName
        {
            get { return viewName; }
            set
            {
                viewName = value;
                _colorId = ColorHelper.Mapping[value];
                OnPropertyChanged("ViewName");
                OnPropertyChanged("ColorId");
            }
        }

        private string _colorId;
        [XmlElement("ColorId")]
        public string ColorId
        {
            get { return _colorId; }
            set { _colorId = value; OnPropertyChanged("ColorId"); }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged(string propertyName)
        {
            var handler = PropertyChanged;
            if (handler != null) handler(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
