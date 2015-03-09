using EnvDTE;
using EnvDTE80;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.VCProjectEngine;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Utilities
{
    public abstract class SolutionUtilities
    {
        public static void AddDirectoryRecursively(VCProject vcProject, string directory)
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


        public static Project GetProject(IVsHierarchy hierarchy)
        {
            object project;
            ErrorHandler.ThrowOnFailure(
                hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out project));
            return (Project)project;
        }

        public static Project TryGetProject(IVsHierarchy hierarchy)
        {
            object project;
            int hr = hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out project);
            return hr == VSConstants.S_OK ? project as Project : null;
        }

        public static void SaveSolution()
        {
            ErrorHandler.ThrowOnFailure(
                Services.Solution.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_SaveIfDirty, null, 0));
        }

        public static string GetSolutionDir()
        {
            var solutionService = Services.Solution;
            string solutionDir, unused;

            ErrorHandler.ThrowOnFailure(
                solutionService.GetSolutionInfo(out solutionDir, out unused, out unused));
            return solutionDir;
        }

        public static bool IsCaideSolution()
        {
            var solutionDir = GetSolutionDir();
            return solutionDir != null && File.Exists(Path.Combine(solutionDir, "caide.ini"));
        }

        public static void CreateAndActivateCppProject(string selectedProblem, string language)
        {
            var dte = Services.DTE;
            var solution = dte.Solution as Solution2;

            var allProjects = solution.Projects.OfType<Project>();
            var project = allProjects.SingleOrDefault(p => p.Name == selectedProblem);
            if (project == null)
            {
                // Create the project
                solution.AddFromTemplate(Paths.CppProjectTemplate,
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

                if (language != "simplecpp")
                {
                    compileTool.AdditionalIncludeDirectories = Path.Combine("$(SolutionDir)", "cpplib");
                    postBuildEventTool.CommandLine = Paths.CaideExe + " make";
                    postBuildEventTool.Description = "Prepare final code for submission";
                    postBuildEventTool.ExcludedFromBuild = false;
                }
                else
                {
                    compileTool.AdditionalIncludeDirectories = "";
                    postBuildEventTool.CommandLine = postBuildEventTool.Description = "";
                }
            }

            SolutionUtilities.SaveSolution();

            dte.Solution.SolutionBuild.StartupProjects = project.UniqueName;

            var allItems = project.ProjectItems.OfType<ProjectItem>();
            var solutionCpp = allItems.Single(i => i.Name == solutionFile);
            var solutionCppWindow = solutionCpp.Open(EnvDTE.Constants.vsViewKindCode);
            solutionCppWindow.Visible = true;
            solutionCppWindow.Activate();
        }

        // Creates cpplib and submission C++ projects
        public static void CreateGeneralCppProjects()
        {
            var solutionDir = GetSolutionDir();
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
            SolutionUtilities.AddDirectoryRecursively(vcProject, cppLibraryDir);

            // Create 'submission' project
            const string submission = "submission";
            project = allProjects.SingleOrDefault(p => p.Name == submission);
            if (project == null)
            {
                solution.AddFromTemplate(Paths.CppProjectTemplate,
                    Destination: Path.Combine(SolutionUtilities.GetSolutionDir(), submission),
                    ProjectName: submission,
                    Exclusive: false);
                allProjects = solution.Projects.OfType<Project>();
                project = allProjects.SingleOrDefault(p => p.Name == submission);
                if (project == null)
                {
                    Logger.LogError("Couldn't create {0} project", submission);
                    return;
                }
            }

            var submissionFile = Path.Combine("..", "submission.cpp");

            if (!project.ProjectItems.OfType<ProjectItem>().Any(item => 
                item.Name.Equals(submissionFile, StringComparison.CurrentCultureIgnoreCase)))
            {
                project.ProjectItems.AddFromFile(submissionFile);
            }


            vcProject = (VCProject)project.Object;
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

            
            SolutionUtilities.SaveSolution();
        }
    }
}
