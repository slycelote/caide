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
    using EnvDTE80;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.VCProjectEngine;
    using slycelote.VsCaide.UI;
    using System.Diagnostics;
    using System.IO;
    using MessageBox = System.Windows.Forms.MessageBox;

    public partial class MainToolWindowControl : System.Windows.Controls.UserControl
    {
        private MainToolWindow mainToolWindow;

        public MainToolWindowControl(MainToolWindow owner)
        {
            InitializeComponent();
            SkipLanguageChangedEvent = true;
            cbProgrammingLanguage.Items.Add("c++");
            cbProgrammingLanguage.Items.Add("simplecpp");
            SkipLanguageChangedEvent = false;
            EnableAll(false);
            this.mainToolWindow = owner;
        }

        private void ReloadProblemList()
        {
            string stdout = CaideExe.Run("getstate", "core", "problem");
            string currentProblem = stdout == null ? null : stdout.Trim();

            var problemNames = new List<string>();

            if (string.Empty == currentProblem)
            {
                problemNames.Add("");
            }

            foreach (var subdir in Directory.EnumerateDirectories(SolutionUtilities.GetSolutionDir()))
            {
                if (Directory.Exists(Path.Combine(subdir, ".caideproblem")) && 
                    File.Exists(Path.Combine(subdir, "problem.ini")))
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

            if (currentProblem == null)
            {
                return;
            }

            cbProblems.SelectedItem = currentProblem;
        }

        private string recentFolder = null;

        private void btnCreateSolution_Click(object sender, RoutedEventArgs e)
        {
            if (SolutionUtilities.IsCaideSolution())
            {
                ReloadProblemList();
            }
            else
            {
                string solutionDir = SolutionUtilities.GetSolutionDir();
                bool newSolution = solutionDir == null;
                if (newSolution)
                {
                    var folderBrowserDialog = new FolderBrowserDialog
                    {
                        Description = "Select solution folder",
                        ShowNewFolderButton = true,
                        RootFolder = Environment.SpecialFolder.Desktop,
                        SelectedPath = recentFolder,
                    };
                    var result = folderBrowserDialog.ShowDialog();
                    if (result != DialogResult.OK)
                        return;
                    solutionDir = recentFolder = folderBrowserDialog.SelectedPath;
                }

                if (null == CaideExe.Run(new[] { "init", "--cpp-use-system-headers" }, loud: true, solutionDir: solutionDir))
                {
                    return;
                }

                if (newSolution)
                {
                    ErrorHandler.ThrowOnFailure(
                        Services.Solution.CreateSolution(solutionDir, "VsCaide", 0)
                    );
                    File.Copy(Path.Combine(solutionDir, "templates", "vs_common.props"),
                              Path.Combine(solutionDir, "vs_common.props"), overwrite: true);
                    SolutionUtilities.SaveSolution();
                }
            }
        }

        private bool IsProjectsLoadingInProgress = false;
        private List<Action> ToDoAfterAllProjectsLoaded = new List<Action>();

        private void AfterProjectsLoaded(Action action)
        {
            bool mustPostpone;
            lock (ToDoAfterAllProjectsLoaded)
            {
                mustPostpone = IsProjectsLoadingInProgress;
                if (mustPostpone)
                    ToDoAfterAllProjectsLoaded.Add(action);
            }
            if (!mustPostpone)
                action();
        }

        public void AllProjects_Loaded()
        {
            lock (ToDoAfterAllProjectsLoaded)
            {
                IsProjectsLoadingInProgress = false;
                ToDoAfterAllProjectsLoaded.ForEach(a => a());
                ToDoAfterAllProjectsLoaded.Clear();
            }
        }

        public void Solution_Opened()
        {
            lock (ToDoAfterAllProjectsLoaded)
            {
                IsProjectsLoadingInProgress = true;
                ToDoAfterAllProjectsLoaded.Clear();
            }

            bool isCaideDirectory = SolutionUtilities.IsCaideSolution();
            EnableAll(isCaideDirectory);

            if (isCaideDirectory)
            {
                var windowFrame = (IVsWindowFrame)mainToolWindow.Frame;
                windowFrame.Show();
                ReloadProblemList();

                AfterProjectsLoaded(() => SolutionUtilities.CreateGeneralCppProjects());
            }
        }

        public void Solution_Closed()
        {
            cbProblems.Items.Clear();
            EnableAll(false);
        }

        private void EnableAll(bool enable)
        {
            btnRun.IsEnabled = btnDebug.IsEnabled = 
                cbProblems.IsEnabled = cbProgrammingLanguage.IsEnabled = btnEditTests.IsEnabled =
                btnAddNewProblem.IsEnabled = btnParseContest.IsEnabled = btnArchive.IsEnabled = enable;
            btnCreateOrReloadCaideSolution.Content = enable ? "Reload problem list" : "Create caide solution";
        }

        private void cbProblems_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            try
            {
                UpdateCurrentProject();
            }
            catch (Exception ex)
            {
                Logger.LogError("Error: {0}", ex);
            }
        }

        private void UpdateCurrentProject()
        {
            string selectedProblem = cbProblems.SelectedItem as string;
            if (string.IsNullOrEmpty(selectedProblem))
                return;

            if (null == CaideExe.Run("checkout", selectedProblem))
            {
                return;
            }

            string stdout = CaideExe.Run("probgetstate", selectedProblem, "problem", "language");
            if (null == stdout)
            {
                return;
            }

            string language = stdout.Trim();
            SetCurrentLanguage(language);

            string[] cppLanguages = new[] { "simplecpp", "cpp", "c++" };
            if (cppLanguages.Contains(language))
            {
                AfterProjectsLoaded(() => SolutionUtilities.CreateAndActivateCppProject(selectedProblem, language));
            }
        }

        private void btnAddNewProblem_Click(object sender, RoutedEventArgs e)
        {
            var problemUrl = PromptDialog.Prompt("Input problem URL or name:", "New problem");
            if (problemUrl == null)
                return;

            if (null == CaideExe.Run(new[] { "problem", problemUrl }, loud: true))
            {
                return;
            }

            ReloadProblemList();
        }

        internal void StartupProject_Changed(IVsHierarchy newStartupProjectHierarchy)
        {
            if (newStartupProjectHierarchy == null)
                return;

            var projectName = SolutionUtilities.GetProject(newStartupProjectHierarchy).Name;
            var currentProblem = (string)cbProblems.SelectedItem;
            if (currentProblem == null || currentProblem.Equals(projectName, StringComparison.CurrentCultureIgnoreCase))
                return;

            if (!IsCaideProblem(projectName))
            {
                // The project doesn't correspond to a problem
                return;
            }

            if (null == CaideExe.Run("checkout", projectName))
            {
                return;
            }

            ReloadProblemList();
        }

        internal void Project_Removed(IVsHierarchy projectHier)
        {
            Project project = SolutionUtilities.TryGetProject(projectHier);
            if (project == null)
                return;
            var projectName = project.Name;
            if (IsCaideProblem(projectName))
            {
                CaideExe.Run("archive", projectName);
                ReloadProblemList();
            }
        }

        private void btnRun_Click(object sender, RoutedEventArgs e)
        {
            Services.DTE.ExecuteCommand("Debug.StartWithoutDebugging");
        }

        private void btnDebug_Click(object sender, RoutedEventArgs e)
        {
            Services.DTE.ExecuteCommand("Debug.Start");
        }

        private void btnParseContest_Click(object sender, RoutedEventArgs e)
        {
            string url = PromptDialog.Prompt("Enter contest URL: ", "Parse contest");
            if (url == null)
                return;
            CaideExe.Run(new[] { "contest", url }, loud: true);
            ReloadProblemList();
        }

        private void btnArchive_Click(object sender, RoutedEventArgs e)
        {
            var currentProblem = (string)cbProblems.SelectedItem;
            if (string.IsNullOrEmpty(currentProblem))
                return;
            var solution = Services.DTE.Solution;
            var project = solution.Projects.OfType<Project>().SingleOrDefault(p => p.Name == currentProblem);
            if (project == null)
            {
                // A problem not tracked by VsCaide
                CaideExe.Run(new[] { "archive", currentProblem }, loud: true);
                ReloadProblemList();
            }
            else
            {
                solution.Remove(project);
                // The problem will be archived on Project_Removed event
            }
        }

        private void btnEditTests_Click(object sender, RoutedEventArgs e)
        {
            string currentProblem = (string)cbProblems.SelectedItem;
            var problemDirectory = Path.Combine(SolutionUtilities.GetSolutionDir(), currentProblem);
            var testCases = TestCase.FromDirectory(problemDirectory);
            testCases = EditTestsWindow.Edit(testCases);
            TestCase.WriteToDirectory(testCases, problemDirectory);
        }

        private bool SkipLanguageChangedEvent = false;
        private void SetCurrentLanguage(string language)
        {
            SkipLanguageChangedEvent = true;
            try
            {
                if (!cbProgrammingLanguage.Items.Contains(language) && !string.IsNullOrEmpty(language))
                    cbProgrammingLanguage.Items.Add(language);
                cbProgrammingLanguage.SelectedItem = language;
            }
            finally
            {
                SkipLanguageChangedEvent = false;
            }
        }

        private void cbProgrammingLanguage_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (!SkipLanguageChangedEvent)
            {
                var language = (string)cbProgrammingLanguage.SelectedItem;
                if (null == CaideExe.Run(new[] { "lang", language }, loud: true))
                {
                    var previousLanguage = (string)e.RemovedItems[0];
                    SetCurrentLanguage(previousLanguage);
                    return;
                }
                UpdateCurrentProject();
            }
        }

        private bool IsCaideProblem(string projectName)
        {
            return cbProblems.Items.Cast<string>().Any(problem =>
                problem.Equals(projectName, StringComparison.CurrentCultureIgnoreCase));
        }

    }
}
