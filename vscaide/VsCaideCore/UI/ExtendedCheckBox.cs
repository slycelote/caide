using System.Windows;
using System.Windows.Controls;

namespace slycelote.VsCaide.Core.UI
{
    /// <summary>
    /// Interaction logic for EditTestsWindow.xaml
    /// </summary>

    public class ExtendedCheckBox : CheckBox
    {
        public bool InvertCheckStateOrder
        {
            get { return (bool)GetValue(InvertCheckStateOrderProperty); }
            set { SetValue(InvertCheckStateOrderProperty, value); }
        }

        // Using a DependencyProperty as the backing store for InvertCheckStateOrder.  This enables animation, styling, binding, etc...
        public static readonly DependencyProperty InvertCheckStateOrderProperty =
            DependencyProperty.Register("InvertCheckStateOrder", typeof(bool), typeof(ExtendedCheckBox), new UIPropertyMetadata(false));

        protected override void OnToggle()
        {
            if (this.InvertCheckStateOrder)
            {
                if (this.IsChecked == true)
                {
                    this.IsChecked = false;
                }
                else if (this.IsChecked == false)
                {
                    this.IsChecked = this.IsThreeState ? null : (bool?)true;
                }
                else
                {
                    this.IsChecked = true;
                }
            }
            else
            {
                base.OnToggle();
            }
        }
    }
}
