using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace slycelote.VsCaide.Core
{
    class NotifyPropertyChangedBase : INotifyPropertyChanged
    {
        public string ChangingProperty { get; private set; } = null;
        public event PropertyChangedEventHandler PropertyChanged;

        public void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        protected bool SetField<T>(ref T field, T value, [CallerMemberName] string propertyName = null)
        {
            if (EqualityComparer<T>.Default.Equals(field, value))
                return false;
            field = value;
            ChangingProperty = propertyName;
            OnPropertyChanged(propertyName);
            ChangingProperty = null;
            return true;
        }
    }
}
