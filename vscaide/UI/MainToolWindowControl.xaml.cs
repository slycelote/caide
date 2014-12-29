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
            cbProgrammingLanguage.Items.Add("cpp");
            cbProgrammingLanguage.Items.Add("simplecpp");
            SkipLanguageChangedEvent = false;
            EnableAll(false);
            this.mainToolWindow = owner;
        }

        private void ReloadProblemList()
        {
            var problemNames = new List<string>();
            foreach (var subdir in Directory.EnumerateDirectories(SolutionUtilities.GetSolutionDir()))
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

            string stdout = RunCaideExe("intgetopt", "core", "problem");
            if (stdout == null)
            {
                return;
            }

            string currentProblem = stdout.Trim();

            cbProblems.SelectedItem = currentProblem;
        }

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
                    };
                    var result = folderBrowserDialog.ShowDialog();
                    if (result != DialogResult.OK)
                        return;
                    solutionDir = folderBrowserDialog.SelectedPath;
                }

                if (null == RunCaideExe(new[] { "init" }, loud: true, solutionDir: solutionDir))
                {
                    return;
                }

                if (newSolution)
                {
                    ErrorHandler.ThrowOnFailure(
                        Services.Solution.CreateSolution(solutionDir, "VsCaide", 0)
                    );
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

                AfterProjectsLoaded(() =>
                {
                    var solutionDir = SolutionUtilities.GetSolutionDir();
                    const string cpplib = "cpplib";
                    var cppLibraryDir = Path.Combine(solutionDir, cpplib);
                    if (!Directory.Exists(cppLibraryDir))
                        return;

                    var dte = Services.DTE;
                    var solution = dte.Solution as Solution2;

                    var allProjects = solution.Projects.OfType<Project>();
                    var project = allProjects.SingleOrDefault(p => p.Name == cpplib);
                    VCProject vcProject;
                    if (project == null)
                    {
                        // Create the project
                        string projectTemplate = solution.GetProjectTemplate("vscaide_vc2013_template.zip", "VC");
                        solution.AddFromTemplate(projectTemplate, Path.Combine(solutionDir, cpplib), cpplib,
                            Exclusive: false);
                        allProjects = solution.Projects.OfType<Project>();
                        project = allProjects.SingleOrDefault(p => p.Name == cpplib);
                        if (project == null)
                        {
                            Logger.LogError("Couldn't create {0} project", cpplib);
                            return;
                        }

                        // Set to static library
                        vcProject = (VCProject)project.Object;
                        var configs = (IVCCollection)vcProject.Configurations;
                        foreach (var conf in configs.OfType<VCConfiguration>())
                        {
                            conf.ConfigurationType = ConfigurationTypes.typeStaticLibrary;
                        }

                    }

                    vcProject = (VCProject)project.Object;

                    // Ensure that all files from the directory are added
                    SolutionUtilities.AddDirectoryRecursively(vcProject, cppLibraryDir);

                    SolutionUtilities.SaveSolution();
                });

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

        private void cbProblems_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            UpdateCurrentProject();
        }

        private void UpdateCurrentProject()
        {
            string selectedProblem = cbProblems.SelectedItem as string;
            if (selectedProblem == null)
                return;

            if (null == RunCaideExe("checkout", selectedProblem))
            {
                return;
            }

            string stdout = RunCaideExe("probgetopt", selectedProblem, "problem", "language");
            if (null == stdout)
            {
                return;
            }

            string language = stdout.Trim();
            SetCurrentLanguage(language);

            string[] cppLanguages = new[] { "simplecpp", "cpp" };
            if (cppLanguages.Contains(language))
            {
                AfterProjectsLoaded(() =>
                {
                    var dte = Services.DTE;
                    var solution = dte.Solution as Solution2;

                    var allProjects = solution.Projects.OfType<Project>();
                    var project = allProjects.SingleOrDefault(p => p.Name == selectedProblem);
                    if (project == null)
                    {
                        // Create the project
                        string projectTemplate = solution.GetProjectTemplate("vscaide_vc2013_template.zip", "VC");
                        solution.AddFromTemplate(projectTemplate,
                            Destination: Path.Combine(SolutionUtilities.GetSolutionDir(), selectedProblem),
                            ProjectName: selectedProblem,
                            Exclusive: false);
                        allProjects = solution.Projects.OfType<Project>();
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

                    var vcProject = (VCProject)project.Object;

                    var cpplibProject = solution.Projects.OfType<Project>().SingleOrDefault(p => p.Name == "cpplib");
                    if (cpplibProject != null)
                    {
                        var references = (VSLangProj.References)vcProject.References;
                        var cpplibReference = references.OfType<VSLangProj.Reference>().SingleOrDefault(r =>
                                r.SourceProject != null && r.SourceProject.UniqueName == cpplibProject.UniqueName);
                        if (language == "cpp")
                        {
                            if (cpplibReference == null)
                            {
                                vcProject.AddProjectReference(cpplibProject);
                            }
                        }
                        else
                        {
                            if (cpplibReference != null)
                            {
                                cpplibReference.Remove();
                            }
                        }
                    }

                    // Ensure current directory of the program debugged is correct
                    var workingDirectory = Path.Combine("$(ProjectDir)", ".caideproblem", "test");
                    var configs = (IVCCollection)vcProject.Configurations;
                    foreach (var conf in configs.OfType<VCConfiguration>())
                    {
                        var debugSettings = (VCDebugSettings)conf.DebugSettings;
                        debugSettings.WorkingDirectory = workingDirectory;

                        var tools = (IVCCollection)conf.Tools; 
                        var linkerTool = (VCLinkerTool)tools.Item("VCLinkerTool");
                        linkerTool.SubSystem = subSystemOption.subSystemConsole;

                        var compileTool = (VCCLCompilerTool)tools.Item("VCCLCompilerTool");
                        compileTool.AdditionalIncludeDirectories = language == "cpp" ? Path.Combine("$(SolutionDir)", "cpplib") : "";
                    }

                    SolutionUtilities.SaveSolution();

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

            if (null == RunCaideExe(new[] { "problem", problemUrl }, loud: true))
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

            if (!cbProblems.Items.Cast<string>().Any(problem => problem.Equals(projectName, StringComparison.CurrentCultureIgnoreCase)))
            {
                // The project doesn't correspond to a problem
                return;
            }

            if (null == RunCaideExe("checkout", projectName))
            {
                return;
            }

            ReloadProblemList();
        }

        private void btnRun_Click(object sender, RoutedEventArgs e)
        {
            Services.DTE.ExecuteCommand("Debug.StartWithoutDebugging");
        }

        private void btnDebug_Click(object sender, RoutedEventArgs e)
        {
            Services.DTE.ExecuteCommand("Debug.Start");
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
                if (null == RunCaideExe(new[] { "lang", language }, loud: true))
                {
                    var previousLanguage = (string)e.RemovedItems[0];
                    SetCurrentLanguage(previousLanguage);
                    return;
                }
                UpdateCurrentProject();
            }
        }

        private static string RunCaideExe(string[] args, bool loud = false, string solutionDir = null)
        {
            if (solutionDir == null)
            {
                solutionDir = SolutionUtilities.GetSolutionDir();
            }

            string stdout, stderr;
            int ret = CaideExe.Execute(args, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                if (loud)
                {
                    MessageBox.Show(string.Format("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr));
                }
                return null;
            }

            return stdout;
        }

        private static string RunCaideExe(params string[] args)
        {
            return RunCaideExe(args, loud: false);
        }
    }
}
