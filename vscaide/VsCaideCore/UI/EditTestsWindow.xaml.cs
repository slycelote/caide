using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace slycelote.VsCaide.Core.UI
{
    /// <summary>
    /// Interaction logic for EditTestsWindow.xaml
    /// </summary>
    class TestsModel : NotifyPropertyChangedBase
    {
        private BindingList<TestCase> testCases;
        public BindingList<TestCase> TestCases
        {
            get { return testCases; }
            set { SetField(ref testCases, value); }
        }

        private TestCase selectedCase;
        public TestCase SelectedCase
        {
            get { return selectedCase; }
            set { SetField(ref selectedCase, value); }
        }

        public bool? AllEnabled
        {
            get
            {
                var checkedCnt = TestCases.Count(tc => tc.IsEnabled);
                if (checkedCnt == TestCases.Count)
                    return true;
                if (checkedCnt == 0)
                    return false;
                return null;
            }
            set
            {
                if (value == null)
                    return;
                foreach (var tc in TestCases)
                    tc.IsEnabled = value.Value;
            }
        }
    }

    public partial class EditTestsWindow : Window
    {
        private TestsModel model = new TestsModel();

        private void chkHeader_Click(object sender, RoutedEventArgs e)
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

            lstTestCases.SelectionChanged += lstTestCases_SelectionChanged;
            btnAdd.Click += btnAdd_Click;
            btnDelete.Click += btnDelete_Click;
            txtInput.TextChanged += txtInput_TextChanged;
            txtOutput.TextChanged += txtOutput_TextChanged;
            chkOutputKnown.Click += chkOutputKnown_Click;
            chkHeader.Click += chkHeader_Click;

            lstTestCases.DataContext = this.model;
            model.TestCases = new BindingList<TestCase>(testCases);
            model.TestCases.ListChanged += (o, i) => model.OnPropertyChanged("AllEnabled");
            model.TestCases.ListChanged += (o, i) => UpdateUI();
            if (testCases.Any())
            {
                model.SelectedCase = testCases[0];
            }
            UpdateUI();
        }

        public static List<TestCase> Edit(List<TestCase> testCases)
        {
            var inst = new EditTestsWindow(testCases);
            inst.ShowDialog();
            return new List<TestCase>(inst.model.TestCases);
        }

        private void UpdateUI()
        {
            TestCase selectedCase = model.SelectedCase;
            btnDelete.IsEnabled = txtInput.IsEnabled = txtOutput.IsEnabled = chkOutputKnown.IsEnabled =
                selectedCase != null;
            if (selectedCase != null)
            {
                txtInput.Text = selectedCase.Input;
                txtOutput.Text = selectedCase.Output;
                chkOutputKnown.IsChecked = selectedCase.OutputIsKnown;
                txtOutput.IsEnabled = selectedCase.OutputIsKnown && selectedCase.IsEnabled;
            }
        }

        private bool DisableEvents = false;

        private void chkOutputKnown_Click(object sender, RoutedEventArgs e)
        {
            if (model.SelectedCase == null || DisableEvents)
                return;
            model.SelectedCase.OutputIsKnown = chkOutputKnown.IsChecked.GetValueOrDefault(true);
            UpdateUI();
        }

        private void txtInput_TextChanged(object sender, TextChangedEventArgs e)
        {
            if (model.SelectedCase == null || DisableEvents)
                return;
            model.SelectedCase.Input = txtInput.Text;
        }

        private void txtOutput_TextChanged(object sender, TextChangedEventArgs e)
        {
            if (model.SelectedCase == null || DisableEvents)
                return;
            model.SelectedCase.Output = txtOutput.Text;
        }

        private void btnDelete_Click(object sender, RoutedEventArgs e)
        {
            int selIndex = lstTestCases.SelectedIndex;
            model.TestCases.RemoveAt(selIndex);
            selIndex = Math.Min(model.TestCases.Count - 1, selIndex);
            lstTestCases.SelectedIndex = selIndex;
        }

        private void btnAdd_Click(object sender, RoutedEventArgs e)
        {
            string newTestName;
            for (int testId = 1; ; ++testId)
            {
                newTestName = "case" + testId;
                if (!model.TestCases.Any(t => StringComparer.CurrentCultureIgnoreCase.Equals(newTestName, t.Name)))
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

            model.TestCases.Add(newTestCase);
            lstTestCases.SelectedItem = newTestCase;
        }
    }
}
