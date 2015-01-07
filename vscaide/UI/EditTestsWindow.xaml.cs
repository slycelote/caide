using System;
using System.Collections.Generic;
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
using System.Windows.Shapes;

namespace slycelote.VsCaide.UI
{
    /// <summary>
    /// Interaction logic for EditTestsWindow.xaml
    /// </summary>
    public partial class EditTestsWindow : Window
    {
        private List<TestCase> TestCases;
        public TestCase SelectedCase { get; private set; }

        public EditTestsWindow(List<TestCase> testCases)
        {
            TestCases = testCases;
            InitializeComponent();
            lstTestCases.Items.Clear();
            foreach (var t in testCases)
            {
                lstTestCases.Items.Add(t);
            }
            if (testCases.Any())
            {
                lstTestCases.SelectedIndex = 0;
            }
            UpdateUI();
        }

        public static List<TestCase> Edit(List<TestCase> testCases)
        {
            var inst = new EditTestsWindow(testCases);
            inst.ShowDialog();
            return inst.TestCases;
        }

        private void UpdateUI()
        {
            btnDelete.IsEnabled = txtInput.IsEnabled = txtOutput.IsEnabled = chkSkipped.IsEnabled = chkOutputKnown.IsEnabled =
                SelectedCase != null;
            if (SelectedCase != null)
            {
                txtInput.Text = SelectedCase.Input;
                txtOutput.Text = SelectedCase.Output;
                chkOutputKnown.IsChecked = SelectedCase.OutputIsKnown;
                chkSkipped.IsChecked = SelectedCase.IsSkipped;
                if (!SelectedCase.OutputIsKnown || SelectedCase.IsSkipped)
                {
                    txtOutput.IsEnabled = false;
                }
            }
        }

        private bool DisableEvents = false;

        private void lstTestCases_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            SelectedCase = (TestCase)lstTestCases.SelectedItem;
            DisableEvents = true;
            UpdateUI();
            DisableEvents = false;
        }

        private void chkSkipped_Click(object sender, RoutedEventArgs e)
        {
            if (SelectedCase == null || DisableEvents)
                return;
            SelectedCase.IsSkipped = chkSkipped.IsChecked.GetValueOrDefault(false);
            UpdateUI();
        }

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
            lstTestCases.Items.RemoveAt(selIndex);
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
                IsSkipped = false,
                OutputIsKnown = true,
            };

            TestCases.Add(newTestCase);
            lstTestCases.Items.Add(newTestCase);
            lstTestCases.SelectedItem = newTestCase;
        }
    }
}
