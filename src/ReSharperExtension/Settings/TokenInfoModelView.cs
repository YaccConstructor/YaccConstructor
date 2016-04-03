using System.ComponentModel;
using System.Xml.Serialization;
using JetBrains.Annotations;

namespace ReSharperExtension.Settings
{
    public class TokenInfoModelView : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        private string tokenName;
        [XmlElement("TokenName")]
        public string TokenName
        {
            get { return tokenName; }
            set
            {
                tokenName = value;
                OnPropertyChanged("TokenName");
            }
        }

        private string _colorId;
        [XmlElement("ColorId")]
        public string ColorId
        {
            get { return _colorId; }
            set
            {
                _colorId = value;
                OnPropertyChanged("ColorId");
            }
        }

        private string leftPair;
        [XmlElement("LeftPair")]
        public string LeftPair
        {
            get { return leftPair; }
            set
            {
                leftPair = value;
                OnPropertyChanged("LeftPair");
            }
        }

        private string rightPair;
        [XmlElement("RightPair")]
        public string RightPair
        {
            get { return rightPair; }
            set
            {
                rightPair = value;
                OnPropertyChanged("RightPair");
            }
        }

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged(string propertyName)
        {
            var handler = PropertyChanged;
            if (handler != null) handler(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
