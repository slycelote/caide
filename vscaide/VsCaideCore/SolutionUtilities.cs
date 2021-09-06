namespace slycelote.VsCaide.Core
{
    using System;
    using System.IO;
    using VsInterface;

    public abstract class SolutionUtilities
    {
        public static volatile bool IgnoreSolutionEvents = false;
        public static DateTime SolutionLoadStart = DateTime.MinValue;
        public static bool HasSolutionLoadCompleted = false;

        public static bool HasSolutionLoaded()
        {
            return HasSolutionLoadCompleted || DateTime.Now - SolutionLoadStart > TimeSpan.FromSeconds(20);
        }

        public static void SaveSolution() => VsImplementation.Services.SaveSolution();

        public static string GetSolutionDir() => VsImplementation.Services.GetSolutionDir();

        public static bool IsCaideSolution()
        {
            var solutionDir = GetSolutionDir();
            return solutionDir != null && File.Exists(Path.Combine(solutionDir, "caide.ini"));
        }
    }

}
