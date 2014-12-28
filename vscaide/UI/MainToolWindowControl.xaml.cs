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
            cbProgrammingLanguage.Items.Add("cpp");
            cbProgrammingLanguage.Items.Add("simplecpp");
            EnableAll(false);
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

            string currentProblem = stdout.Trim();

            cbProblems.SelectedItem = currentProblem;
        }

        private void btnCreateSolution_Click(object sender, RoutedEventArgs e)
        {
            if (IsCaideSolution)
            {
                ReloadProblemList();
            }
            else
            {
                string solutionDir = SolutionDir;
                bool newSolution = solutionDir == null;
                if (newSolution)
                {
                    var folderBrowserDialog = new FolderBrowserDialog
                    {
                        Description = "Select solution folder",
                        ShowNewFolderButton = true,
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
                    SaveSolution();
                }
            }
        }

        private bool IsProjectsLoadingInProgress = false;
        private List<Action> ToDoAfterAllProjectsLoaded = new List<Action>();

        private void AfterProjectsLoaded(Action action)
        {
            lock (ToDoAfterAllProjectsLoaded)
            {
                if (IsProjectsLoadingInProgress)
                    ToDoAfterAllProjectsLoaded.Add(action);
                else
                    action();
            }
        }

        public void Solution_Opened()
        {
            lock (ToDoAfterAllProjectsLoaded)
            {
                IsProjectsLoadingInProgress = true;
                ToDoAfterAllProjectsLoaded.Clear();
            }
            bool isCaideDirectory = IsCaideSolution;
            EnableAll(isCaideDirectory);
            if (isCaideDirectory)
            {
                IVsWindowFrame windowFrame = (IVsWindowFrame)mainToolWindow.Frame;
                windowFrame.Show();
                ReloadProblemList();
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
                cbProblems.IsEnabled = cbProgrammingLanguage.IsEnabled =
                btnAddNewProblem.IsEnabled = enable;
            btnCreateOrReloadCaideSolution.Content = enable ? "Reload problem list" : "Create caide solution";
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
                AfterProjectsLoaded(() =>
                {
                    var dte = Services.DTE;
                    var solution = dte.Solution as Solution2;

                    var allProjects = dte.Solution.Projects.OfType<Project>();
                    var project = allProjects.SingleOrDefault(p => p.Name == selectedProblem);
                    if (project == null)
                    {
                        // Create the project
                        string projectTemplate = solution.GetProjectTemplate("vscaide_vc2013_template.zip", "VC");
                        solution.AddFromTemplate(projectTemplate, Path.Combine(solutionDir, selectedProblem), selectedProblem,
                            Exclusive: false);
                        allProjects = dte.Solution.Projects.OfType<Project>();
                        project = allProjects.SingleOrDefault(p => p.Name == selectedProblem);
                        if (project == null)
                        {
                            Logger.LogError("Couldn't create {0} project", selectedProblem);
                            return;
                        }
                    }

                    // Ensure that the project contains necessary files
                    var solutionFile = string.Format(@"{0}.cpp", selectedProblem);
                    var testFile = string.Format(@"{0}_test.cpp", selectedProblem);

                    foreach (var fileName in new[]{solutionFile, testFile})
                    {
                        if (!project.ProjectItems.OfType<ProjectItem>().Any(item => item.Name.Equals(fileName, StringComparison.CurrentCultureIgnoreCase)))
                        {
                            project.ProjectItems.AddFromFile(fileName);
                        }
                    }

                    // Ensure current directory of the program debugged is correct
                    var workingDirectory = Path.Combine("$(ProjectDir)", ".caideproblem", "test");
                    var vcProject = project.Object as VCProject;
                    IVCCollection configs = (IVCCollection)vcProject.Configurations;
                    foreach (var conf in configs.OfType<VCConfiguration>())
                    {
                        var debugSettings = (VCDebugSettings)conf.DebugSettings;
                        debugSettings.WorkingDirectory = workingDirectory;

                        var tools = (Microsoft.VisualStudio.VCProjectEngine.IVCCollection) conf.Tools; 
                        var linkerTool = (VCLinkerTool)tools.Item("VCLinkerTool");
                        linkerTool.SubSystem = subSystemOption.subSystemConsole;
                    }

                    SaveSolution();

                    dte.Solution.SolutionBuild.StartupProjects = project.UniqueName;

                    var allItems = project.ProjectItems.OfType<ProjectItem>();
                    var solutionCpp = allItems.Single(i => i.Name == solutionFile);
                    var solutionCppWindow = solutionCpp.Open(EnvDTE.Constants.vsViewKindCode);
                    solutionCppWindow.Visible = true;
                    solutionCppWindow.Activate();
                });
            }

        }

        private void btnAddNewProblem_Click(object sender, RoutedEventArgs e)
        {
            var problemUrl = PromptDialog.Prompt("Input problem URL or name:", "New problem");
            if (problemUrl == null)
                return;
            var solutionDir = SolutionDir;
            string stdout, stderr;
            int ret = CaideExe.Execute(new[] { "problem", problemUrl }, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                MessageBox.Show(string.Format("Coudln't create the problem.\n{0}\n{1}", stdout, stderr));
                return;
            }

            ReloadProblemList();
        }

        internal void StartupProject_Changed(IVsHierarchy newStartupProjectHierarchy)
        {
            if (newStartupProjectHierarchy == null)
                return;

            var projectName = GetProject(newStartupProjectHierarchy).Name;
            var currentProblem = (string)cbProblems.SelectedItem;
            if (currentProblem == null || currentProblem.Equals(projectName, StringComparison.CurrentCultureIgnoreCase))
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
                var solutionService = Services.Solution;
                string solutionDir, unused;

                ErrorHandler.ThrowOnFailure(
                    solutionService.GetSolutionInfo(out solutionDir, out unused, out unused));
                return solutionDir;
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

        private void SaveSolution()
        {
            ErrorHandler.ThrowOnFailure(
                Services.Solution.SaveSolutionElement(0, null, 0)
            );
        }

        private void btnRun_Click(object sender, RoutedEventArgs e)
        {
            Services.CommandWindow.ExecuteCommand("Debug.StartWithoutDebugging");
        }

        private void btnDebug_Click(object sender, RoutedEventArgs e)
        {
            Services.CommandWindow.ExecuteCommand("Debug.Start");
        }

    }
}
