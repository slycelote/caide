using Microsoft.VisualStudio.Shell.Interop;
using slycelote.VsCaide.Utilities;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Forms;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace slycelote.VsCaide
{
    using Microsoft.VisualStudio;
    using System.IO;
    using MessageBox = System.Windows.Forms.MessageBox;

    public partial class MainToolWindowControl : System.Windows.Controls.UserControl
    {
        private MainToolWindow mainToolWindow;

        public MainToolWindowControl(MainToolWindow owner)
        {
            InitializeComponent();
            cbProgrammingLanguage.Items.Add("cpp");
            cbProgrammingLanguage.Items.Add("simplecpp");
            cbProgrammingLanguage.IsEnabled = cbProblems.IsEnabled = btnAddNewProblem.IsEnabled = false;
            this.mainToolWindow = owner;
        }

        private void ReloadProblemList()
        {
            IVsSolution solutionService = Services.Solution;
            string solutionDir, unused;
            ErrorHandler.ThrowOnFailure(
                solutionService.GetSolutionInfo(out solutionDir, out unused, out unused)
            );
            var problemNames = new List<string>();
            foreach (var subdir in Directory.EnumerateDirectories(solutionDir))
            {
                if (Directory.Exists(Path.Combine(subdir, ".caideproblem")))
                {
                    problemNames.Add(Path.GetFileName(subdir.TrimEnd(Path.DirectorySeparatorChar)));
                }
            }

            problemNames.Sort(StringComparer.CurrentCultureIgnoreCase);
            cbProblems.Items.Clear();
            foreach (var problem in problemNames)
            {
                cbProblems.Items.Add(problem);
            }

            string stdout, stderr;
            int ret = CaideExe.Execute(new[] { "intgetopt", "core", "problem" }, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Error code: {0}\n{1}\n{2}", ret, stdout, stderr);
                return;
            }

            cbProblems.SelectedItem = stdout.Trim();
        }

        private void btnCreateSolution_Click(object sender, RoutedEventArgs e)
        {
            IVsSolution solutionService = Services.Solution;
            string solutionDir, slnFile, userOptsFile;
            ErrorHandler.ThrowOnFailure(
                solutionService.GetSolutionInfo(out solutionDir, out slnFile, out userOptsFile)
            );
            bool newSolution = solutionDir == null;
            if (newSolution)
            {
                var folderBrowserDialog = new FolderBrowserDialog
                {
                    Description = "Select solution folder",
                    ShowNewFolderButton = true,
                    RootFolder = Environment.SpecialFolder.Personal,
                };
                var result = folderBrowserDialog.ShowDialog();
                if (result != DialogResult.OK)
                    return;
                solutionDir = folderBrowserDialog.SelectedPath;
            }

            string stdout, stderr;
            int caideErrorCode = CaideExe.Execute(new[] { "init" }, solutionDir, out stdout, out stderr);
            if (caideErrorCode != 0)
            {
                MessageBox.Show(string.Format("Failed to initialize caide project. Error code: {0}\n{1}\n{2}",
                    caideErrorCode, stdout, stderr));
                return;
            }

            if (newSolution)
            {
                ErrorHandler.ThrowOnFailure(
                    solutionService.CreateSolution(solutionDir, "VsCaide", 0)
                );
                ErrorHandler.ThrowOnFailure(
                    solutionService.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, null, 0)
                );
            }
        }

        public void Solution_Opened()
        {
            var solutionService = Services.Solution;
            string solutionDir, unused;

            ErrorHandler.ThrowOnFailure(
                solutionService.GetSolutionInfo(out solutionDir, out unused, out unused));

            bool isCaideDirectory = File.Exists(Path.Combine(solutionDir, "caide.ini"));
            btnCreateSolution.IsEnabled = !isCaideDirectory;
            cbProblems.IsEnabled = cbProgrammingLanguage.IsEnabled = btnAddNewProblem.IsEnabled = isCaideDirectory;
            if (isCaideDirectory)
            {
                IVsWindowFrame windowFrame = (IVsWindowFrame)mainToolWindow.Frame;
                windowFrame.Show();
                ReloadProblemList();
            }
        }

        public void Solution_Closed()
        {
            btnCreateSolution.IsEnabled = true;
            cbProblems.Items.Clear();
            cbProblems.IsEnabled = cbProgrammingLanguage.IsEnabled = btnAddNewProblem.IsEnabled = false;
        }

        private void cbProblems_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            string selectedProblem = cbProblems.SelectedItem as string;
            if (selectedProblem == null)
                return;

            var solutionService = Services.Solution;
            string solutionDir, unused;

            ErrorHandler.ThrowOnFailure(
                solutionService.GetSolutionInfo(out solutionDir, out unused, out unused));
            if (solutionDir == null)
                return;


            string stdout, stderr;
            int ret = CaideExe.Execute(new[] { "probgetopt", selectedProblem, "problem", "language" }, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                return;
            }

            string language = stdout.Trim();
            if (!cbProgrammingLanguage.Items.Contains(language) && !string.IsNullOrEmpty(language))
                cbProgrammingLanguage.Items.Add(language);
            cbProgrammingLanguage.SelectedItem = language;
        }
    }
}
