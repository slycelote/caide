using slycelote.VsCaide.Core.UI;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using slycelote.VsCaide.VsInterface;

namespace slycelote.VsCaide.Core
{
    class OneWayConverter<From, To> : IValueConverter
    {
        private readonly Func<From, To> Func;

        public OneWayConverter(Func<From, To> func)
        {
            this.Func = func;
        }

        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return Func((From)value);
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    public partial class VsCaideMainWindowControl : UserControl
    {
        private readonly IWindow mainWindow;
        private readonly VsCaideModel model;
        private CHelperServer CHelperServer;
        private FileSystemWatcher fsWatcher;
        private DateTime lastChange = DateTime.MinValue;
        private readonly IVsServices services;
        private readonly IProjectManager projectManager;

        /// <summary>
        /// Initializes a new instance of the <see cref="VsCaideMainWindowControl"/> class.
        /// </summary>
        public VsCaideMainWindowControl(IWindow vsCaideMainWindow)
        {
            services = VsImplementation.Services;
            services.ThrowIfNotOnUIThread();
            Log("ctor");
            this.InitializeComponent();

            mainWindow = vsCaideMainWindow;
            model = new VsCaideModel();
            projectManager = services.GetProjectManager();

            var sortedProblemsView = new CollectionViewSource { Source = model.Problems };
            sortedProblemsView.SortDescriptions.Add(new SortDescription(nameof(CaideProblem.Name), ListSortDirection.Ascending));
            cbProblems.SetBinding(ComboBox.ItemsSourceProperty, new Binding
            {
                Source = sortedProblemsView,
                Mode = BindingMode.OneWay,
            });

            cbProblems.DisplayMemberPath = nameof(CaideProblem.Name);

            cbProblems.SetBinding(ComboBox.SelectedValueProperty, new Binding(nameof(VsCaideModel.SelectedProblem))
            {
                Source = model,
                Mode = BindingMode.OneWay,
            });

            cbProblems.SelectionChanged += CbProblems_SelectionChanged;

            cbProgrammingLanguage.ItemsSource = model.ProgrammingLanguages;
            cbProgrammingLanguage.SetBinding(ComboBox.SelectedValueProperty, new Binding(nameof(VsCaideModel.ProgrammingLanguage))
            {
                Source = model,
                Mode = BindingMode.OneWay,
            });
            cbProgrammingLanguage.SelectionChanged += CbProgrammingLanguage_SelectionChanged;

            var visibleIfNotCaideSolution = new Binding(nameof(model.IsCaideSolution))
            {
                Source = model,
                Converter = new OneWayConverter<bool, Visibility>(isCaide => isCaide ? Visibility.Hidden : Visibility.Visible)
            };

            var visibleIfCaideSolution = new Binding(nameof(model.IsCaideSolution))
            {
                Source = model,
                Converter = new BooleanToVisibilityConverter()
            };

            var isCaideSolution = new Binding(nameof(model.IsCaideSolution)) { Source = model };

            btnReload.SetBinding(UIElement.VisibilityProperty, visibleIfCaideSolution);
            btnCreateSolution.SetBinding(UIElement.VisibilityProperty, visibleIfNotCaideSolution);
            foreach (var control in new List<FrameworkElement> { btnAddProblem, btnParseContest, btnEditTests, btnRunTests, btnDebugTests, btnArchiveProblem, cbProblems, cbProgrammingLanguage, menuEditConfig, menuArchiveProblems })
            {
                control.SetBinding(UIElement.IsEnabledProperty, isCaideSolution);
            }

            btnAddProblem.Click += BtnAdd_Click;
            btnParseContest.Click += BtnParseContest_Click;
            btnEditTests.Click += BtnEditTests_Click;
            btnCreateSolution.Click += BtnCreate_Click;
            btnReload.Click += BtnReload_Click;
            btnRunTests.Click += BtnRunTests_Click;
            btnDebugTests.Click += BtnDebugTests_Click;
            btnArchiveProblem.Click += BtnArchiveProblem_Click;
            btnAdvanced.Click += BtnAdvanced_Click;

            menuEditConfig.Click += MenuEditConfig_Click;
            menuArchiveProblems.Click += MenuArchiveProblems_Click;
        }

        private TResource GetResource<TResource>(object resourceKey)
            where TResource: class
        {
            try
            {
                return this.FindResource(resourceKey) as TResource;
            }
            catch (ResourceReferenceKeyNotFoundException)
            {
                return null;
            }
            catch (ArgumentNullException)
            {
                return null;
            }
        }

        public void SetStyles(object buttonStyleResourceKey,
            object comboBoxStyleResourceKey,
            object backgroundBrushResourceKey, object foregroundBrushResourceKey)
        {
            var btnStyle = GetResource<Style>(buttonStyleResourceKey);
            foreach (var btn in new[] {btnAddProblem, btnAdvanced, btnArchiveProblem, btnCreateSolution,
                btnDebugTests, btnEditTests, btnParseContest, btnReload, btnRunTests})
            {
                btn.Style = btnStyle;
            }

            var cbStyle = GetResource<Style>(comboBoxStyleResourceKey);
            foreach (var cb in new [] {cbProblems, cbProgrammingLanguage})
            {
                cb.Style = cbStyle;
            }

            var fgColor = GetResource<Brush>(foregroundBrushResourceKey);
            foreach (var lbl in new[] {lblSelectedLanguage, lblSelectedProblem})
            {
                lbl.Foreground = fgColor;
            }

            //menuAdvanced.Background = GetResource<Brush>(backgroundBrushResourceKey);
            //menuAdvanced.Foreground = fgColor;
        }

        private void MenuArchiveProblems_Click(object sender, RoutedEventArgs e) 
            => ExceptionUtilities.CatchAll(() =>
        {
            var selection = ArchiveProblemsWindow.SelectProblems(model.Problems);
            if (selection == null || selection.SelectedProblems.Count == 0)
            {
                return;
            }

            services.ThrowIfNotOnUIThread();
            foreach (var problem in selection.SelectedProblems)
            {
                ArchiveProblem(problem, remove: selection.RemoveInsteadOfArchiving);
            }

            ReloadProblemList();
        });

        private void MenuEditConfig_Click(object sender, RoutedEventArgs e) =>
            ExceptionUtilities.CatchAll(() =>
        {
            services.ThrowIfNotOnUIThread();
            var configFile = Path.Combine(SolutionUtilities.GetSolutionDir(), "caide.ini");
            services.ExecuteCommand("File.OpenFile", configFile);
        });

        private void BtnAdvanced_Click(object sender, RoutedEventArgs e) 
            => ExceptionUtilities.CatchAll(() =>
        {
            btnAdvanced.ContextMenu.IsOpen = true;
        });

        private void BtnEditTests_Click(object sender, RoutedEventArgs e) 
            => ExceptionUtilities.CatchAll(() =>
        {
            var currentProblem = model.SelectedProblem;
            var problemDirectory = Path.Combine(SolutionUtilities.GetSolutionDir(), currentProblem.Name);
            var testCases = TestCase.FromDirectory(problemDirectory);
            testCases = EditTestsWindow.Edit(testCases);
            TestCase.WriteToDirectory(testCases, problemDirectory);
        });

        private void BtnParseContest_Click(object sender, RoutedEventArgs e) 
            => ExceptionUtilities.CatchAll(() =>
        {
            string url = PromptDialog.Prompt("Enter contest URL: ", "Parse contest");
            if (url == null)
                return;
            CaideExe.Run(new[] { "contest", url }, loud: Loudness.LOUD);
        });

        /************************
         * User actions handlers
         ************************/

        private void BtnArchiveProblem_Click(object sender, RoutedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            var currentProblem = model.SelectedProblem;
            if (currentProblem == null || string.IsNullOrEmpty(currentProblem.Name))
                return;
            services.ThrowIfNotOnUIThread();
            ArchiveProblem(currentProblem);
        });

        private void BtnDebugTests_Click(object sender, RoutedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            services.ThrowIfNotOnUIThread();
            services.ExecuteCommand("Debug.Start");
        });

        private void BtnRunTests_Click(object sender, RoutedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            services.ThrowIfNotOnUIThread();
            services.ExecuteCommand("Debug.StartWithoutDebugging");
        });

        private void BtnReload_Click(object sender, RoutedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            ReloadProblemList();
        });

        private void BtnAdd_Click(object sender, RoutedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            var tupleResult = PromptDialog.Prompt(
                "Input problem URL or name:", "New problem", optionalInputLabel: "Override problem ID");
            if (tupleResult == null)
                return;

            string overrideId = tupleResult.Item2;
            if (string.IsNullOrWhiteSpace(overrideId))
                overrideId = null;
            string problemUrl = tupleResult.Item1;
            if (string.IsNullOrWhiteSpace(problemUrl))
            {
                problemUrl = overrideId;
                overrideId = null;
            }

            if (problemUrl == null)
            {
                MessageBox.Show("Either problem URL or ID need to be specified");
                return;
            }

            var args = new List<string> { "problem", problemUrl };
            if (overrideId != null)
            {
                args.Add("--id");
                args.Add(overrideId);
            }

            CaideExe.Run(args.ToArray(), loud: Loudness.LOUD);
            // UI update will be done in fsWatcher callback.
        });

        private string recentFolder = null;

        private void BtnCreate_Click(object sender, RoutedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            services.ThrowIfNotOnUIThread();
            string solutionDir = SolutionUtilities.GetSolutionDir();
            bool newSolution = solutionDir == null;
            if (newSolution)
            {
                var folderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog
                {
                    Description = "Select solution folder",
                    ShowNewFolderButton = true,
                    RootFolder = Environment.SpecialFolder.MyComputer,
                    SelectedPath = recentFolder,
                };
                var result = folderBrowserDialog.ShowDialog();
                if (result != System.Windows.Forms.DialogResult.OK)
                    return;
                solutionDir = recentFolder = folderBrowserDialog.SelectedPath;
            }

            if (null == CaideExe.Run(new[] { "init" }, loud: Loudness.LOUD, solutionDir: solutionDir))
            {
                return;
            }

            if (newSolution)
            {
                services.ThrowOnFailure(
                    services.CreateSolution(solutionDir, "VsCaide")
                );
                File.Copy(Path.Combine(solutionDir, "templates", "vs_common.props"),
                          Path.Combine(solutionDir, "vs_common.props"), overwrite: true);
                services.SaveSolution();
            }
        });

        private void CbProblems_SelectionChanged(object sender, SelectionChangedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            bool isUserInitiated = model.ChangingProperty != nameof(VsCaideModel.SelectedProblem);
            if (!isUserInitiated)
            {
                return;
            }

            if (e.AddedItems.Count == 0)
            {
                Log("Error: e.AddedItems is empty");
                return;
            }

            var newProblem = e.AddedItems[0] as CaideProblem;
            var previousProblem = e.RemovedItems.Count == 0 ? null : e.RemovedItems[0];
            if (null == CaideExe.Run(new[] { "checkout", newProblem.Name }, loud: Loudness.LOUD))
            {
                // Switch model value to null and back to force update of combobox selection.
                model.SelectedProblem = null;
                model.SelectedProblem = previousProblem as CaideProblem;
                // Probably, the problem doesn't exist anymore. Reload problem list to confirm.
                // TODO: Add/check caide.exe error code / message.
                ReloadProblemList();
            }
        });

        private void CbProgrammingLanguage_SelectionChanged(object sender, SelectionChangedEventArgs e)
            => ExceptionUtilities.CatchAll(() =>
        {
            bool isUserInitiated = model.ChangingProperty != nameof(VsCaideModel.ProgrammingLanguage);
            if (!isUserInitiated)
            {
                return;
            }

            var newLanguage = e.AddedItems[0] as string;
            var previousLanguage = e.RemovedItems.Count == 0 ? null : e.RemovedItems[0];
            if (null == CaideExe.Run(new[] { "lang", newLanguage }, loud: Loudness.LOUD))
            {
                // Switch model value to null and back to force update of combobox selection.
                model.ProgrammingLanguage = null;
                model.ProgrammingLanguage = previousLanguage as string;
                return;
            }

            UpdateCurrentProject(newLanguage);
        });

        private async void FsWatcher_Fired(object sender, FileSystemEventArgs e)
        {
            await ExceptionUtilities.CatchAllAsync(async () =>
            {
                var fsWatcher = sender as FileSystemWatcher;
                if (fsWatcher != this.fsWatcher)
                {
                    return;
                }

                if (e.ChangeType == WatcherChangeTypes.Changed)
                {
                    lock (fsWatcher)
                    {
                        if (DateTime.Now <= lastChange + TimeSpan.FromSeconds(2))
                            return;
                        lastChange = DateTime.Now;
                    }
                }

                if (e.ChangeType == WatcherChangeTypes.Renamed)
                {
                    return;
                }

                string fileName = e.Name.ToLower();
                if ((e.ChangeType == WatcherChangeTypes.Created || e.ChangeType == WatcherChangeTypes.Deleted) &&
                    fileName != ".caideproblem" && fileName != "problem.ini")
                {
                    return;
                }

                await services.SwitchToMainThreadAsync();
                Logger.Trace("[!] {0} {1}", fileName, e.ChangeType);

                if (!fsWatcher.Path.StartsWith(SolutionUtilities.GetSolutionDir(), StringComparison.CurrentCultureIgnoreCase))
                {
                    fsWatcher.EnableRaisingEvents = false;
                    return;
                }

                ReloadProblemList();
            });
        }

        /************************
         * VS events handlers
         ************************/

        public void OnBeforeOpenSolution()
            => ExceptionUtilities.CatchAll(() =>
        {
            LogMethod();
            SolutionUtilities.SolutionLoadStart = DateTime.Now;
        });

        public void OnAfterLoadedSolution()
        {
            services.ThrowIfNotOnUIThread();
            LogMethod();
            model.IsCaideSolution = SolutionUtilities.IsCaideSolution();
            if (!model.IsCaideSolution)
                return;

            string checkUpdates =
                CaideExe.Run(new[] { "getopt", "core", "check_updates" }, loud: Loudness.QUIET);
            if (checkUpdates == null)
            {
                var configFilePath = Path.Combine(SolutionUtilities.GetSolutionDir(), "caide.ini");
                var lines = File.ReadAllLines(configFilePath);
                var newLines = new List<string>();
                foreach (var line in lines)
                {
                    newLines.Add(line);
                    if (line.Trim() == "[core]")
                    {
                        newLines.Add("check_updates: false");
                    }
                }
                File.WriteAllLines(configFilePath, newLines.ToArray());
            }

            mainWindow.Show();

            ReloadProblemList();

            if (CHelperServer != null)
            {
                CHelperServer.Stop();
                CHelperServer = null;
            }

            string enableChelperServerStr =
                CaideExe.Run(new[] { "getopt", "vscaide", "enable_http_server" }, loud: Loudness.QUIET) ?? "1";
            if (IsTrue(enableChelperServerStr))
            {
                CHelperServer = new CHelperServer();
            }
            else
            {
                Logger.LogMessage("Disabling CHelper HTTP server due to a setting in caide.ini");
            }

            EnableFsWatcher(false);

            string path = Path.Combine(SolutionUtilities.GetSolutionDir(), ".caide");
            this.fsWatcher = new FileSystemWatcher(path, "config")
            {
                IncludeSubdirectories = false,
                NotifyFilter = NotifyFilters.LastWrite,
            };
            this.fsWatcher.Changed += FsWatcher_Fired;

            EnableFsWatcher(true);

            projectManager.CreateCppLibProject();
        }

        public void OnAfterBackgroundSolutionLoadComplete()
        {
            services.ThrowIfNotOnUIThread();
            LogMethod();
            SolutionUtilities.HasSolutionLoadCompleted = true;
        }

        public void OnBeforeBackgroundSolutionLoadBegins()
        {
            services.ThrowIfNotOnUIThread();
            LogMethod();
        }

        public void OnAfterCloseSolution()
        {
            services.ThrowIfNotOnUIThread();
            LogMethod();
            EnableFsWatcher(false);
            fsWatcher = null;

            this.model.IsCaideSolution = false;
            this.model.Problems.Clear();
            this.model.SelectedProblem = null;
            if (CHelperServer != null)
            {
                CHelperServer.Stop();
                CHelperServer = null;
            }
        }

        public void OnStartupProjectChanged(IProject newStartupProject)
        {
            services.ThrowIfNotOnUIThread();
            LogMethod();
            if (newStartupProject == null)
                return;

            var projectName = newStartupProject.Name;
            var currentProblem = model.SelectedProblem;
            if (currentProblem != null && projectName.Equals(currentProblem.Name, StringComparison.CurrentCultureIgnoreCase))
                return;

            if (!IsCaideProblem(projectName))
            {
                // The project doesn't correspond to a problem
                return;
            }

            CaideExe.Run("checkout", projectName);
            // UI update will be done in fsWatcher callback.
        }

        public void OnBeforeDeleteProject(IProject project)
        {
            services.ThrowIfNotOnUIThread();
            LogMethod();
            if (IsCaideProblem(project.Name))
            {
                // Try to mitigate a mysterious error 'Unsatisified (sic!) constraints: folder not empty'.
                Thread.Sleep(TimeSpan.FromSeconds(0.5));
                CaideExe.Run("archive", project.Name);
                string projectDirectory = Path.Combine(SolutionUtilities.GetSolutionDir(), project.Name);
                if (Directory.Exists(projectDirectory))
                    Directory.Delete(projectDirectory, recursive: true);
                SolutionUtilities.SaveSolution();
            }
        }

        /************************
         * Utility functions
         ************************/

        private static void Log(string s)
        {
            Logger.Trace("{0}", s);
        }

        private static void LogMethod([CallerMemberName] string s = null)
        {
            Log(s);
        }

        private void EnableFsWatcher(bool enable)
        {
            if (fsWatcher != null)
                fsWatcher.EnableRaisingEvents = enable;
        }

        private void ArchiveProblem(CaideProblem problem, bool remove = false)
        {
            services.ThrowIfNotOnUIThread();
            var project = services.Projects.SingleOrDefault(p =>
                problem.Name.Equals(p.Name, StringComparison.CurrentCultureIgnoreCase));

            if (project != null)
            {
                services.RemoveProject(project);
                services.SaveSolution();
            }

            if (remove)
            {
                var problemDir = Path.Combine(services.GetSolutionDir(), problem.Name);
                Directory.Delete(problemDir, recursive: true);
            }
            else
            {
                CaideExe.Run(new[] { "archive", problem.Name }, loud: Loudness.LOUD);
            }
        }

        private void ReloadProblemList()
        {
            LogMethod();
            string stdout = CaideExe.Run("getstate", "core", "problem");
            string currentProblemName = stdout?.Trim();

            var problemNames = new List<string>();

            if (string.IsNullOrEmpty(currentProblemName))
            {
                currentProblemName = "";
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
            var problems = new List<CaideProblem>();
            CaideProblem currentProblem = null;
            foreach (var problemName in problemNames)
            {
                var problem = new CaideProblem { Name = problemName };
                if (problemName == currentProblemName)
                    currentProblem = problem;
                problems.Add(problem);
            }

            model.SetProblems(problems);
            model.SelectedProblem = currentProblem;
            UpdateCurrentProject();
        }

        private void UpdateCurrentProject(string language = null)
        {
            var selectedProblem = model.SelectedProblem;
            if (selectedProblem == null || string.IsNullOrEmpty(selectedProblem.Name))
                return;

            if (language == null)
            {
                string stdout = CaideExe.Run("probgetstate", selectedProblem.Name, "problem", "language");
                if (null == stdout)
                {
                    return;
                }

                language = stdout.Trim();
            }

            model.ProgrammingLanguage = language;

            string[] cppLanguages = new[] { "simplecpp", "cpp", "c++" };
            string[] csLanguages = new[] { "c#", "csharp" };
            if (cppLanguages.Contains(language))
            {
                projectManager.CreateAndActivateCppProject(selectedProblem.Name, language);
            }
            else if (csLanguages.Contains(language))
            {
                projectManager.CreateAndActivateCSharpProject(selectedProblem.Name);
            }
            else
            {
                // TODO: Delete the project?
            }
        }

        private bool IsCaideProblem(string projectName)
        {
            return model.Problems.Any(problem =>
                problem.Name.Equals(projectName, StringComparison.CurrentCultureIgnoreCase));
        }

        private bool IsTrue(string configValue)
        {
            return new[] { "yes", "1", "true" }.Contains(configValue.ToLowerInvariant().Trim());
        }
    }
}
