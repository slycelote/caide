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
    using EnvDTE;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.VCProjectEngine;
    using System.Diagnostics;
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
            var problemNames = new List<string>();
            foreach (var subdir in Directory.EnumerateDirectories(SolutionDir))
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
            int ret = CaideExe.Execute(new[] { "intgetopt", "core", "problem" }, SolutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Error code: {0}\n{1}\n{2}", ret, stdout, stderr);
                return;
            }

            cbProblems.SelectedItem = stdout.Trim();
        }

        private void btnCreateSolution_Click(object sender, RoutedEventArgs e)
        {
            string solutionDir = SolutionDir;
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
                    Services.Solution.CreateSolution(solutionDir, "VsCaide", 0)
                );
                ErrorHandler.ThrowOnFailure(
                    Services.Solution.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, null, 0)
                );
            }
        }

        public void Solution_Opened()
        {
            bool isCaideDirectory = IsCaideSolution;
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

            string solutionDir = SolutionDir;
            if (solutionDir == null)
                return;

            string stdout, stderr;
            int ret = CaideExe.Execute(new[] { "checkout", selectedProblem }, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                return;
            }

            ret = CaideExe.Execute(new[] { "probgetopt", selectedProblem, "problem", "language" }, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                return;
            }

            string language = stdout.Trim();
            if (!cbProgrammingLanguage.Items.Contains(language) && !string.IsNullOrEmpty(language))
                cbProgrammingLanguage.Items.Add(language);
            cbProgrammingLanguage.SelectedItem = language;

            string[] cppLanguages = new[] { "simplecpp", "cpp" };
            if (cppLanguages.Contains(language))
            {
                var dte = Services.DTE;
                var allProjects = dte.Solution.Projects.OfType<Project>();
                var project = allProjects.Single(p => p.Name == selectedProblem);
                dte.Solution.SolutionBuild.StartupProjects = project.UniqueName;
                
                var allItems = project.ProjectItems.OfType<ProjectItem>();
                var solutionCpp = allItems.Single(i => i.Name == selectedProblem + ".cpp");
                //bool isOpen = solutionCpp.get_IsOpen(EnvDTE.Constants.vsViewKindCode);
                var solutionCppWindow = solutionCpp.Open(EnvDTE.Constants.vsViewKindCode);
                // FIXME: If this happens during opening of the solution, a second window with the file opens
                solutionCppWindow.Visible = true;
                solutionCppWindow.Activate();
            }

        }

        // Test
        private void btnAddNewProblem_Click(object sender, RoutedEventArgs e)
        {
            //var dte = Services.DTE;
            var dte = (EnvDTE.DTE)Package.GetGlobalService(typeof(EnvDTE.DTE));
            var allProjects = dte.Solution.Projects.OfType<Project>();
            var project = allProjects.Single(p => p.Name == (string)cbProblems.SelectedItem);
            object[] uniqueNames = { project.UniqueName };
            //dte.Solution.SolutionBuild.StartupProjects = uniqueNames;
            dte.Solution.SolutionBuild.StartupProjects = project.UniqueName;
            Debug.WriteLine("...");
        }

        internal void StartupProject_Changed(IVsHierarchy newStartupProjectHierarchy)
        {
            if (newStartupProjectHierarchy == null)
                return;

            var projectName = GetProject(newStartupProjectHierarchy).Name;
            var currentProblem = (string)cbProblems.SelectedItem;
            if (currentProblem.Equals(projectName, StringComparison.CurrentCultureIgnoreCase))
                return;

            if (!cbProblems.Items.Cast<string>().Any(problem => problem.Equals(projectName, StringComparison.CurrentCultureIgnoreCase)))
            {
                // The project doesn't correspond to a problem
                return;
            }

            string solutionDir = SolutionDir;
            if (solutionDir == null)
                return;

            string stdout, stderr;
            int ret = CaideExe.Execute(new[] { "checkout", projectName }, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                return;
            }

            ReloadProblemList();
        }

        private static Project GetProject(IVsHierarchy hierarchy)
        {
            object project;
            ErrorHandler.ThrowOnFailure(
                hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out project));
            return project as Project;
        }

        private string SolutionDir
        {
            get
            {
                {
                    var solutionService = Services.Solution;
                    string solutionDir, unused;

                    ErrorHandler.ThrowOnFailure(
                        solutionService.GetSolutionInfo(out solutionDir, out unused, out unused));
                    return solutionDir;
                }
            }
        }

        private bool IsCaideSolution
        {
            get
            {
                var solutionDir = SolutionDir;
                return solutionDir != null && File.Exists(Path.Combine(solutionDir, "caide.ini"));
            }
        }
    }
}
