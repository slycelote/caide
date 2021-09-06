namespace slycelote.VsCaide.Core
{
    using System;
    using System.Diagnostics;
    using System.Windows;

    public enum Loudness
    {
        QUIET,
        NORMAL,
        LOUD
    }

    public abstract class CaideExe
    {
        public const int EXIT_CODE_TIMEOUT = -4451;
        public const int EXIT_CODE_EXCEPTION = -4452;

        public static string Run(string[] args, Loudness loud = Loudness.NORMAL, string solutionDir = null)
        {
            if (solutionDir == null)
            {
                solutionDir = SolutionUtilities.GetSolutionDir();
            }

            string stdout, stderr;
            int ret = CaideExe.Execute(args, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                if (loud >= Loudness.NORMAL)
                {
                    Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                }
                if (loud >= Loudness.LOUD)
                {
                    MessageBox.Show(string.Format("{0}\n{1}", stdout, stderr));
                }
                return null;
            }

            return stdout;
        }

        public static string Run(params string[] args)
        {
            return Run(args, loud: Loudness.NORMAL);
        }


        public static int Execute(string[] args, string directory, out string stdout, out string stderr)
        {
            Logger.Trace("{0}", "caide.exe " + string.Join(" ", args));
            if (Paths.CaideExe == null)
            {
                stderr = "Couldn't find caide.exe";
                stdout = "";
                return 1;
            }

            var psi = new ProcessStartInfo
            {
                FileName = Paths.CaideExe,
                Arguments = CommandLine.CreateCommandLine(args),
                WorkingDirectory = directory,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            };

            try
            {
                using (var process = Process.Start(psi))
                {
                    var stdOutTask = process.StandardOutput.ReadToEndAsync();
                    var stdErrTask = process.StandardError.ReadToEndAsync();

                    var waitTime = 30 * 1000;
                    if (process.WaitForExit(waitTime))
                    {
                        stdout = stdOutTask.Result;
                        stderr = stdErrTask.Result;
                        return process.ExitCode;
                    }

                    stdout = "";
                    stderr = "";

                    return EXIT_CODE_TIMEOUT;
                }
            }
            catch (Exception e)
            {
                stdout = "";
                stderr = e.Message;
                return EXIT_CODE_EXCEPTION;
            }
        }

    }
}
