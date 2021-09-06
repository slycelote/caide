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
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace slycelote.VsCaide.Core.UI
{
    class ArchiveProblemsSelection : NotifyPropertyChangedBase
    {
        public List<CaideProblem> SelectedProblems;

        private bool removeInsteadOfArchiving;
        public bool RemoveInsteadOfArchiving
        {
            get { return removeInsteadOfArchiving; }
            set { SetField(ref removeInsteadOfArchiving, value); }
        }
    }
    public partial class ArchiveProblemsWindow : Window
    {
        private ArchiveProblemsSelection model = new ArchiveProblemsSelection();

        private ArchiveProblemsWindow(IEnumerable<CaideProblem> problems)
        {
            InitializeComponent();
            model.RemoveInsteadOfArchiving = false;
            model.SelectedProblems = new List<CaideProblem>();

            chkDelete.SetBinding(CheckBox.IsCheckedProperty,
                new Binding(nameof(ArchiveProblemsSelection.RemoveInsteadOfArchiving)) { Source = model, Mode = BindingMode.TwoWay });
            lstProblems.ItemsSource = problems.ToList();
            lstProblems.DisplayMemberPath = nameof(CaideProblem.Name);

            btnCancel.Click += BtnCancel_Click;
            btnOk.Click += BtnOk_Click;
            btnSelectAll.Click += BtnSelectAll_Click;
        }

        private void BtnSelectAll_Click(object sender, RoutedEventArgs e)
        {
            foreach (var item in lstProblems.Items)
            {
                lstProblems.SelectedItems.Add(item);
            }
        }

        private void BtnOk_Click(object sender, RoutedEventArgs e)
        {
            model.SelectedProblems = lstProblems.SelectedItems.Cast<CaideProblem>().ToList();
            DialogResult = true;
            Close();
        }

        private void BtnCancel_Click(object sender, RoutedEventArgs e)
        {
            model.SelectedProblems.Clear();
            DialogResult = false;
            Close();
        }

        internal static ArchiveProblemsSelection SelectProblems(IEnumerable<CaideProblem> problems)
        {
            var inst = new ArchiveProblemsWindow(problems);
            bool? ok = inst.ShowDialog();
            return ok is bool && (bool)ok ? inst.model : null;
        }
    }
}
