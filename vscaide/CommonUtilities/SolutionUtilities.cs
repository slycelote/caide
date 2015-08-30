using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
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
        public static volatile bool IgnoreSolutionEvents = false;

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
