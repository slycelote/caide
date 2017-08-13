using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace slycelote.VsCaide.UI
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

    public partial class EditTestsWindow : Window, INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;
        protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChangedEventHandler handler = PropertyChanged;
            if (handler != null)
                handler(this, new PropertyChangedEventArgs(propertyName));
        }
        protected bool SetField<T>(ref T field, T value, [CallerMemberName] string propertyName = null)
        {
            if (EqualityComparer<T>.Default.Equals(field, value))
                return false;
            field = value;
            OnPropertyChanged(propertyName);
            return true;
        }

        private BindingList<TestCase> testCases;
        public BindingList<TestCase> TestCases { get { return testCases; } set { SetField(ref testCases, value); }  }
        private TestCase selectedCase;
        public TestCase SelectedCase {
            get { return selectedCase; }
            set { SetField(ref selectedCase, value); }
        }
        public bool? AllEnabled {
            get {
                var checkedCnt = TestCases.Count(tc => tc.IsEnabled);
                if (checkedCnt == TestCases.Count)
                    return true;
                if (checkedCnt == 0)
                    return false;
                return null;
            }
            set {
                if (value == null)
                    return;
                foreach (var tc in TestCases)
                    tc.IsEnabled = value.Value;
            }
        }

        private void OnCbClicked (object sender, RoutedEventArgs e)
        {
            var cb = e.Source as CheckBox;
            if (!cb.IsChecked.HasValue)
                cb.IsChecked = false;
        }

        private void lstTestCases_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            DisableEvents = true;
            UpdateUI();
            DisableEvents = false;
        }

        public EditTestsWindow(List<TestCase> testCases)
        {
            InitializeComponent();
            lstTestCases.DataContext = this;
            TestCases = new BindingList<TestCase>(testCases);
            TestCases.ListChanged += (o, i) => OnPropertyChanged("AllEnabled");
            TestCases.ListChanged += (o, i) => UpdateUI ();
            if (testCases.Any())
            {
                SelectedCase = testCases[0];
            }
            UpdateUI();
        }

        public static List<TestCase> Edit(List<TestCase> testCases)
        {
            var inst = new EditTestsWindow(testCases);
            inst.ShowDialog();
            return new List<TestCase> (inst.TestCases);
        }

        private void UpdateUI()
        {
            btnDelete.IsEnabled = txtInput.IsEnabled = txtOutput.IsEnabled = chkOutputKnown.IsEnabled =
                SelectedCase != null;
            if (SelectedCase != null)
            {
                txtInput.Text = SelectedCase.Input;
                txtOutput.Text = SelectedCase.Output;
                chkOutputKnown.IsChecked = SelectedCase.OutputIsKnown;
                txtOutput.IsEnabled = (SelectedCase.OutputIsKnown && SelectedCase.IsEnabled);
            }
        }

        private bool DisableEvents = false;

        private void chkOutputKnown_Click(object sender, RoutedEventArgs e)
        {
            if (SelectedCase == null || DisableEvents)
                return;
            SelectedCase.OutputIsKnown = chkOutputKnown.IsChecked.GetValueOrDefault(true);
            UpdateUI();
        }

        private void txtInput_TextChanged(object sender, TextChangedEventArgs e)
        {
            if (SelectedCase == null || DisableEvents)
                return;
            SelectedCase.Input = txtInput.Text;
        }

        private void txtOutput_TextChanged(object sender, TextChangedEventArgs e)
        {
            if (SelectedCase == null || DisableEvents)
                return;
            SelectedCase.Output = txtOutput.Text;
        }

        private void btnDelete_Click(object sender, RoutedEventArgs e)
        {
            int selIndex = lstTestCases.SelectedIndex;
            TestCases.RemoveAt(selIndex);
            selIndex = Math.Min(TestCases.Count - 1, selIndex);
            lstTestCases.SelectedIndex = selIndex;
        }

        private void btnAdd_Click(object sender, RoutedEventArgs e)
        {
            string newTestName;
            for (int testId = 1; ; ++testId)
            {
                newTestName = "case" + testId;
                if (!TestCases.Any(t => StringComparer.CurrentCultureIgnoreCase.Equals(newTestName, t.Name)))
                {
                    break;
                }
            }

            var newTestCase = new TestCase
            {
                Name = newTestName,
                Input = "",
                Output = "",
                IsEnabled = true,
                OutputIsKnown = true,
            };

            TestCases.Add(newTestCase);
            lstTestCases.SelectedItem = newTestCase;
        }
    }
}
