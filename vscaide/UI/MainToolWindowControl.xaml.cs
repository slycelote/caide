using slycelote.VsCaide.Utilities;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace slycelote.VsCaide
{
    public partial class MainToolWindowControl : UserControl
    {
        public MainToolWindowControl()
        {
            InitializeComponent();
        }

        // Test method
        private void MainToolWindowUserControl_MouseDoubleClick(object sender, MouseButtonEventArgs e)
        {
            string stdout, stderr;
            int exitCode = CaideExe.Execute(new[] {"help"}, @"C:\code\caide\libcaide\test", out stdout, out stderr);
            MessageBox.Show(string.Format("Exit code: {0},\nstdout:\n{1}\nstderr:\n{2}", exitCode, stdout, stderr));
        }
    }
}
