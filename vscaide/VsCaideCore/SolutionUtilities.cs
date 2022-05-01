namespace slycelote.VsCaide.Core
{
    using System;
    using System.IO;
    using VsInterface;

    public static class SolutionUtilities
    {
        private static volatile bool ignoreSolutionEvents = false;
        public static DateTime SolutionLoadStart = DateTime.MinValue;
        public static bool HasSolutionLoadCompleted = false;

        private sealed class IgnoreResetter : IDisposable
        {
            public void Dispose() => ignoreSolutionEvents = false;
        }

        public static bool IgnoreSolutionEvents { get => ignoreSolutionEvents; }

        public static IDisposable IgnoringSolutionEvents()
        {
            ignoreSolutionEvents = true;
            return new IgnoreResetter();
        }

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
