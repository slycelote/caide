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

    }
}
