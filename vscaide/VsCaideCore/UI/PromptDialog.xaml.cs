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
    /// <summary>
    /// Interaction logic for PromptDialog.xaml
    /// </summary>
    public partial class PromptDialog : Window
    {
        public PromptDialog(string question, string title, string defaultValue = "",
            string optionalInputLabel = null)
        {
            InitializeComponent();
            this.Loaded += PromptDialog_Loaded;
            this.btnOk.Click += btnOk_Click;
            this.btnCancel.Click += btnCancel_Click;
            txtQuestion.Text = question;
            Title = title;
            txtResponse.Text = defaultValue;
            if (optionalInputLabel == null)
            {
                lblOptionalInputName.Visibility = Visibility.Collapsed;
                txtOptionalInput.Visibility = Visibility.Collapsed;
            }
            else
            {
                lblOptionalInputName.Content = optionalInputLabel;
                lblOptionalInputName.Target = txtOptionalInput;
            }
        }

        private void PromptDialog_Loaded(object sender, RoutedEventArgs e)
        {
            txtResponse.Focus();
        }

        public static string Prompt(string question, string title,
            string defaultValue = "")
        {
            return Prompt(question, title, null, defaultValue).Item1;
        }

        public static Tuple<string, string> Prompt(string question, string title,
            string optionalInputLabel, string defaultValue = "")
        {
            var inst = new PromptDialog(question, title, defaultValue, optionalInputLabel);
            inst.ShowDialog();
            return inst.DialogResult == true ?
                Tuple.Create(inst.txtResponse.Text, inst.txtOptionalInput.Text) :
                null;
        }

        private void btnOk_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = true;
            Close();
        }

        private void btnCancel_Click(object sender, RoutedEventArgs e)
        {
            Close();
        }
    }
}
