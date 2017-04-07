using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;

using VsInterface;
using slycelote.VsCaide.Utilities;

using EnvDTE;
using EnvDTE80;
using VSLangProj;

using Microsoft.VisualStudio.VCProjectEngine;

namespace slycelote.VsCaide.VsSpecific
{
    /// <summary>
    /// This is a common implementation for all Visual Studio versions.
    /// The only difference is which version of VCProjectEngine assembly the DLL references.
    /// </summary>
    public class ProjectManager : IProjectManager
    {
        private static void AddDirectoryRecursively(VCProject vcProject, string directory)
        {
            var requiredDirectories = new HashSet<string>(
                Directory.EnumerateFiles(directory, "*.h", SearchOption.AllDirectories)
                .Union(Directory.EnumerateFiles(directory, "*.cpp", SearchOption.AllDirectories))
                .Select(f => Paths.NormalizePath(Path.GetDirectoryName(f)))
            );

            Action<dynamic, string> processDir = null; // Argument is either VCProject or VCFilter
            processDir = (parent, fullPath) =>
            {
                foreach (var header in Directory.EnumerateFiles(fullPath, "*.h", SearchOption.TopDirectoryOnly))
                {
                    var files = (IVCCollection)vcProject.Files;
                    if (!files.OfType<VCFile>().Where(f => f.FileType == eFileType.eFileTypeCppHeader && f.FullPath == header).Any())
                        parent.AddFile(header);
                }

                foreach (var cppFile in Directory.EnumerateFiles(fullPath, "*.cpp", SearchOption.TopDirectoryOnly))
                {
                    var files = (IVCCollection)vcProject.Files;
                    if (!files.OfType<VCFile>().Where(f => f.FileType == eFileType.eFileTypeCppCode && f.FullPath == cppFile).Any())
                        parent.AddFile(cppFile);
                }

                foreach (var dir in Directory.EnumerateDirectories(fullPath))
                {
                    if (!requiredDirectories.Any(d => d.StartsWith(dir, StringComparison.CurrentCultureIgnoreCase)))
                        continue;

                    var subFilters = (IVCCollection)parent.Filters;
                    var subFolder = Path.GetFileName(dir);
                    var child = subFilters.OfType<VCFilter>().SingleOrDefault(f => f.Name == subFolder);
                    if (child == null)
                        child = parent.AddFilter(subFolder);
                    processDir(child, dir);
                }
            };

            processDir(vcProject, directory);
        }

        private Project RecreateProjectOfCorrectType(string projectName, ProjectType projectType)
        {
            var dte = Services.DTE;
            var solution = dte.Solution as Solution2;
            string solutionDir = SolutionUtilities.GetSolutionDir();
            string projectDir = Path.Combine(solutionDir, projectName);

            var allProjects = solution.Projects.OfType<Project>();
            var oldProject = allProjects.SingleOrDefault(p => p.Name == projectName);
            if (oldProject != null && projectType.Belongs(oldProject))
                return oldProject;

            ProjectType oldProjectType = oldProject == null ? null : GetProjectType(oldProject);
            bool needBackup = projectType.RequiresEmptyDirectoryForCreation ||
                (oldProject != null && (oldProjectType == null || oldProjectType.RequiresEmptyDirectoryForCreation));

            Project newProject = null;

            string tempDir = null;
            try
            {
                // Backup current directory
                if (needBackup)
                {
                    tempDir = Path.Combine(Path.GetTempPath(), "vscaide", projectName);
                    if (Directory.Exists(tempDir))
                    {
                        Directory.Delete(tempDir, recursive: true);
                    }

                    // Don't keep VS files
                    var filesToDelete = new[] { ".vcproj", ".vcxproj", ".csproj", ".user", ".exe", ".pdb" };
                    var foldersToDelete = new[] { "obj", "Release", "Debug" };
                    Func<FileInfo, bool> filter = fi =>
                        foldersToDelete.Any(f => fi.FullName.StartsWith(Path.Combine(projectDir, f), StringComparison.CurrentCultureIgnoreCase)) ||
                        !filesToDelete.Contains(fi.Extension, StringComparer.CurrentCultureIgnoreCase);

                    FileUtility.DirectoryCopy(projectDir, tempDir, copySubDirs: true, fileFilter: filter);
                }

                // Remove the old project
                if (oldProject != null)
                {
                    SolutionUtilities.IgnoreSolutionEvents = true;
                    try
                    {
                        solution.Remove(oldProject);
                    }
                    finally
                    {
                        SolutionUtilities.IgnoreSolutionEvents = false;
                    }
                }

                // Create the project
                if (projectType.RequiresEmptyDirectoryForCreation)
                {
                    FileUtility.RemoveFiles(projectDir);
                }
                solution.AddFromTemplate(projectType.ProjectTemplate,
                    Destination: Path.Combine(SolutionUtilities.GetSolutionDir(), projectName),
                    ProjectName: projectName,
                    Exclusive: false);

                allProjects = solution.Projects.OfType<Project>();
                newProject = allProjects.SingleOrDefault(p => p.Name == projectName);
                if (newProject == null)
                {
                    Logger.LogError("Couldn't create {0} project", projectName);
                }
            }
            finally
            {
                // Restore backup
                if (tempDir != null)
                {
                    FileUtility.DirectoryCopy(tempDir, projectDir, copySubDirs: true, fileFilter: null);
                    Directory.Delete(tempDir, recursive: true);
                }
            }

            return newProject;
        }

        public void CreateAndActivateCSharpProject(string selectedProblem)
        {
            Project project = RecreateProjectOfCorrectType(selectedProblem, CSharpProjectType);
            if (project == null)
            {
                return;
            }

            var vsProject = (VSProject)project.Object;

            // Ensure that the project contains necessary files
            var solutionFile = string.Format(@"{0}.cs", selectedProblem);
            var testFile = string.Format(@"{0}_test.cs", selectedProblem);

            var solutionDir = SolutionUtilities.GetSolutionDir();
            var projectDir = Path.Combine(solutionDir, selectedProblem);
            foreach (var fileName in new[]{solutionFile, testFile})
            {
                if (!project.ProjectItems.OfType<ProjectItem>().Any(item => item.Name.Equals(fileName, StringComparison.CurrentCultureIgnoreCase)))
                {
                    project.ProjectItems.AddFromFile(Path.Combine(projectDir, fileName));
                }
            }

            var dte = Services.DTE;

            dte.Solution.SolutionBuild.StartupProjects = project.UniqueName;

            var allItems = project.ProjectItems.OfType<ProjectItem>();
            var solutionCs = allItems.Single(i => i.Name == solutionFile);
            var solutionCsWindow = solutionCs.Open(EnvDTE.Constants.vsViewKindCode);
            solutionCsWindow.Visible = true;
            solutionCsWindow.Activate();

            CreateSubmissionCsProject();

            SolutionUtilities.SaveSolution();
        }

        public void CreateAndActivateCppProject(string selectedProblem, string language)
        {
            Project project = RecreateProjectOfCorrectType(selectedProblem, CppProjectType);

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

            var dte = Services.DTE;
            var solution = dte.Solution as Solution2;

            var cpplibProject = solution.Projects.OfType<Project>().SingleOrDefault(p => p.Name == "cpplib");
            if (cpplibProject != null)
            {
                var references = (VSLangProj.References)vcProject.References;
                var cpplibReference = references.OfType<VSLangProj.Reference>().SingleOrDefault(r =>
                        r.SourceProject != null && r.SourceProject.UniqueName == cpplibProject.UniqueName);
                if (language != "simplecpp")
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
            var configs = (IVCCollection)vcProject.Configurations;
            foreach (var conf in configs.OfType<VCConfiguration>())
            {
                conf.OutputDirectory = @"$(ProjectDir)\$(Configuration)\";
                var debugSettings = (VCDebugSettings)conf.DebugSettings;
                debugSettings.WorkingDirectory = "$(ProjectDir)";

                var tools = (IVCCollection)conf.Tools;
                var linkerTool = (VCLinkerTool)tools.Item("VCLinkerTool");
                linkerTool.SubSystem = subSystemOption.subSystemConsole;

                var compileTool = (VCCLCompilerTool)tools.Item("VCCLCompilerTool");
                var postBuildEventTool = (VCPostBuildEventTool)tools.Item("VCPostBuildEventTool");

                postBuildEventTool.CommandLine = "\"" + Paths.CaideExe + "\" make";
                postBuildEventTool.Description = "Prepare final code for submission";
                postBuildEventTool.ExcludedFromBuild = false;

                if (language != "simplecpp")
                {
                    compileTool.AdditionalIncludeDirectories = Path.Combine("$(SolutionDir)", "cpplib");
                }
                else
                {
                    compileTool.AdditionalIncludeDirectories = "";
                }
            }

            dte.Solution.SolutionBuild.StartupProjects = project.UniqueName;

            var allItems = project.ProjectItems.OfType<ProjectItem>();
            var solutionCpp = allItems.Single(i => i.Name == solutionFile);
            var solutionCppWindow = solutionCpp.Open(EnvDTE.Constants.vsViewKindCode);
            solutionCppWindow.Visible = true;
            solutionCppWindow.Activate();

            CreateSubmissionCppProject();

            SolutionUtilities.SaveSolution();
        }

        public void CreateCppLibProject()
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
                solution.AddFromTemplate(Paths.CppProjectTemplate, Path.Combine(solutionDir, cpplib), cpplib,
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
                    conf.OutputDirectory = @"$(ProjectDir)\$(Configuration)\";
                }

            }

            vcProject = (VCProject)project.Object;

            // Ensure that all files from the directory are added
            AddDirectoryRecursively(vcProject, cppLibraryDir);

            SolutionUtilities.SaveSolution();
        }

        public void CreateSubmissionCppProject()
        {
            Project project = RecreateProjectOfCorrectType("submission", CppProjectType);

            var submissionFile = Path.Combine("..", "submission.cpp");

            if (!project.ProjectItems.OfType<ProjectItem>().Any(item =>
                submissionFile.Equals(item.Name, StringComparison.CurrentCultureIgnoreCase)))
            {
                project.ProjectItems.AddFromFile(submissionFile);
            }

            var vcProject = (VCProject)project.Object;
            var submissionConfigs = (IVCCollection)vcProject.Configurations;
            foreach (var conf in submissionConfigs.OfType<VCConfiguration>())
            {
                conf.OutputDirectory = @"$(ProjectDir)\$(Configuration)\";
                var debugSettings = (VCDebugSettings)conf.DebugSettings;
                debugSettings.WorkingDirectory = "$(ProjectDir)";

                var tools = (IVCCollection)conf.Tools;
                var linkerTool = (VCLinkerTool)tools.Item("VCLinkerTool");
                linkerTool.SubSystem = subSystemOption.subSystemConsole;
            }
        }

        public void CreateSubmissionCsProject()
        {
            Project project = RecreateProjectOfCorrectType("submission", CSharpProjectType);

            // Create submission.cs file if it's missing.
            var submissionCs = Path.Combine(SolutionUtilities.GetSolutionDir(), "submission.cs");
            if (!File.Exists(submissionCs))
            {
                File.WriteAllText(submissionCs, "");
            }

            if (!project.ProjectItems.OfType<ProjectItem>().Any(item =>
                "submission.cs".Equals(item.Name, StringComparison.CurrentCultureIgnoreCase)))
            {
                project.ProjectItems.AddFromFile(submissionCs);
            }
        }

        private readonly static ProjectType CSharpProjectType = new ProjectType
        {
            Belongs = p => p.Kind == PrjKind.prjKindCSharpProject,
            ProjectTemplate = Paths.CSharpProjectTemplate,
            RequiresEmptyDirectoryForCreation = true,
        };

        private readonly static ProjectType CppProjectType = new ProjectType
        {
            Belongs = p => p.Object is VCProject,
            ProjectTemplate = Paths.CppProjectTemplate,
            RequiresEmptyDirectoryForCreation = false,
        };


        private static ProjectType GetProjectType(Project p)
        {
            foreach (var projectType in new[] { CSharpProjectType, CppProjectType })
            {
                if (projectType.Belongs(p))
                    return projectType;
            }
            return null;
        }
    }

    internal class ProjectType
    {
        public Func<Project, bool> Belongs { get; set; }
        public string ProjectTemplate { get; set; }

        // C# project needs an empty directory; and viceversa, when a C# project
        // gets removed, the whole directory is cleared.
        public bool RequiresEmptyDirectoryForCreation { get; set; }
    }
}
